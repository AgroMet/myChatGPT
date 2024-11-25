import streamlit as st
import sqlite3
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

# Function to initialize the SQLite database
def init_db():
    conn = sqlite3.connect("conversations.db")
    cursor = conn.cursor()
    cursor.execute(
        """
        CREATE TABLE IF NOT EXISTS conversations (
            chat_id TEXT PRIMARY KEY,
            chat_name TEXT,
            messages TEXT
        )
        """
    )
    conn.commit()
    conn.close()

# Function to save conversations to the database
def save_conversations():
    conn = sqlite3.connect("conversations.db")
    cursor = conn.cursor()
    for chat_id, messages in st.session_state.conversations.items():
        chat_name = st.session_state.chat_names.get(chat_id, chat_id)
        messages_str = str(messages)  # Convert list of dicts to string
        cursor.execute(
            """
            INSERT OR REPLACE INTO conversations (chat_id, chat_name, messages)
            VALUES (?, ?, ?)
            """,
            (chat_id, chat_name, messages_str),
        )
    conn.commit()
    conn.close()

# Function to load conversations from the database
def load_conversations():
    conn = sqlite3.connect("conversations.db")
    cursor = conn.cursor()
    cursor.execute("SELECT chat_id, chat_name, messages FROM conversations")
    rows = cursor.fetchall()
    conn.close()

    conversations = {}
    chat_names = {}
    for chat_id, chat_name, messages_str in rows:
        conversations[chat_id] = eval(messages_str)  # Convert string back to list of dicts
        chat_names[chat_id] = chat_name
    return conversations, chat_names

# Initialize SQLite database
init_db()

# Load conversations into session state
if "conversations" not in st.session_state:
    st.session_state.conversations, st.session_state.chat_names = load_conversations()

# Ensure default conversation exists if not in session state
if "default" not in st.session_state.conversations:
    st.session_state.conversations["default"] = []
    st.session_state.chat_names["default"] = "Default Chat"

if "current_chat" not in st.session_state:
    st.session_state.current_chat = "default"

if "user_input" not in st.session_state:
    st.session_state.user_input = ""

if "selected_model" not in st.session_state:
    st.session_state.selected_model = "qwen2.5-coder:3b"

# Sidebar for model selection
st.sidebar.selectbox(
    "Select the model:",
    options=["qwen2.5-coder:3b", "qwen2.5-coder:7b", "qwen2.5-coder:14b"],
    key="selected_model",
)

# Sidebar for managing conversations
st.sidebar.title("Conversations")

if st.sidebar.button("New Conversation"):
    new_chat_id = f"chat_{len(st.session_state.conversations) + 1}"
    st.session_state.conversations[new_chat_id] = []
    st.session_state.chat_names[new_chat_id] = new_chat_id  # Default name
    st.session_state.current_chat = new_chat_id
    save_conversations()  # Save immediately to database

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
    save_conversations()  # Save immediately to database

# Main chat interface
st.title("Ollama Chat Interface")
current_chat_id = st.session_state.current_chat
current_chat_name = st.session_state.chat_names[current_chat_id]
st.subheader(f"Conversation: {current_chat_name}")

chat_history = st.session_state.conversations[current_chat_id]

# Define the colors for user prompts and Ollama responses
user_color = "#470E0E"  # Light blue background for user prompts
ollama_color = "#101807"  # Light green background for Ollama responses

# Display chat history with colors
for message in chat_history:
    if message["type"] == "user":
        st.markdown(f'<div style="background-color:{user_color}; padding: 10px; border-radius: 5px;">You: {message["content"]}</div>', unsafe_allow_html=True)
    else:
        st.markdown(f'<div style="background-color:{ollama_color}; padding: 10px; border-radius: 5px;"> {message["content"]}</div>', unsafe_allow_html=True)

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
        save_conversations()  # Save immediately to database

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
