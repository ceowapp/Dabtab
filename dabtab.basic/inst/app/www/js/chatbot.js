// chatbot.js
document.addEventListener("DOMContentLoaded", function() {
      // Add this JavaScript code before the closing </body> tag
        const chatbotModal = document.getElementById('chatbotModal');
        const openChatbot = document.getElementById('chatbotToggle');
        const closeChatbot = document.getElementById('closeChatbot');

        openChatbot.addEventListener('click', () => {
            chatbotModal.style.display = 'block';
        });

        openChatbot.addEventListener('click', () => {
        chatbotModal.style.display = 'block';
        // Set the dimensions of the modal content to match the chatbot iframe
        chatbotModal.querySelector('.modal-content').style.width = `${chatbotIframe.offsetWidth}px`;
        chatbotModal.querySelector('.modal-content').style.height = `${chatbotIframe.offsetHeight}px`;
         });
        
        closeChatbot.addEventListener('click', () => {
            chatbotModal.style.display = 'none';
        });

        window.addEventListener('click', (event) => {
            if (event.target === chatbotModal) {
                chatbotModal.style.display = 'none';
            }
        });

    
});


