import streamlit as st
import subprocess

# Function to query the Ollama model
def query_ollama(prompt, model):
    try:
        command = ["ollama", "run", model, prompt]
        result = subprocess.run(command, capture_output=True, text=True)

        if result.returncode != 0:
            return f"Error: {result.stderr.strip()}"

        return result.stdout.strip()
    except Exception as e:
        return f"Error: {e}"


# Dropdown menu for selecting the model
st.sidebar.selectbox(
    "Select the model:",
    options=["qwen2.5-coder:3b", "qwen2.5-coder:7b", "qwen2.5-coder:14b"],
    key="selected_model",
)

# Initialize session state for chat history and input handling
if "conversations" not in st.session_state:
    st.session_state.conversations = {"default": []}  # Default conversation
if "current_chat" not in st.session_state:
    st.session_state.current_chat = "default"
if "user_input" not in st.session_state:
    st.session_state.user_input = ""
if "selected_model" not in st.session_state:
    st.session_state.selected_model = "qwen2.5-coder:3b"
if "chat_names" not in st.session_state:
    st.session_state.chat_names = {"default": "Default Chat"}  # Map chat IDs to names

# Sidebar for managing conversations
st.sidebar.title("Conversations")

if st.sidebar.button("New Conversation"):
    new_chat_id = f"chat_{len(st.session_state.conversations) + 1}"
    st.session_state.conversations[new_chat_id] = []
    st.session_state.chat_names[new_chat_id] = new_chat_id  # Use default name initially
    st.session_state.current_chat = new_chat_id

# Display chat names in the sidebar and enable renaming
for chat_id, chat_name in st.session_state.chat_names.items():
    if st.sidebar.button(chat_name, key=chat_id):
        st.session_state.current_chat = chat_id

st.sidebar.subheader("Rename Chat")
new_name = st.sidebar.text_input(
    "Enter new name:", value=st.session_state.chat_names[st.session_state.current_chat]
)
if st.sidebar.button("Rename"):
    st.session_state.chat_names[st.session_state.current_chat] = new_name

# Main chat interface
st.title("Ollama Chat Interface")
current_chat_id = st.session_state.current_chat
current_chat_name = st.session_state.chat_names[current_chat_id]
st.subheader(f"Conversation: {current_chat_name}")

chat_history = st.session_state.conversations[current_chat_id]

for message in chat_history:
    if message["type"] == "user":
        st.write(f"You: {message['content']}")
    else:
        st.write(f"Ollama: {message['content']}")

# Function to handle input submission
def submit_input():
    user_input = st.session_state.user_input.strip()
    selected_model = st.session_state.selected_model
    if user_input:
        chat_history.append({"type": "user", "content": user_input})
        response = query_ollama(user_input, selected_model)
        chat_history.append({"type": "bot", "content": response})
        st.session_state.conversations[current_chat_id] = chat_history
        st.session_state.user_input = ""  # Clear input after submission

# Form for user input
with st.form(key="chat_form"):
    st.text_area(
        "Enter your prompt:",
        value=st.session_state.user_input,
        key="user_input",
        placeholder="Type your prompt here...",
        height=150,
    )
    submit_button = st.form_submit_button("Send", on_click=submit_input)
