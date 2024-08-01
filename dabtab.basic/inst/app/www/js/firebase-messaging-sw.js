import { getMessaging, getToken } from "firebase/messaging/sw";
import { onBackgroundMessage } from "firebase/messaging/sw";

function requestPermissionAndSubscribeToTopic(topic, eventDetails) {
  console.log('Requesting permission...');
  Notification.requestPermission().then((permission) => {
    if (permission === 'granted') {
      console.log('Notification permission granted.');

      const messaging = getMessaging();

      getToken(messaging, { vapidKey: 'YOUR_VAPID_KEY' }).then((currentToken) => {
        if (currentToken) {
          // Send the token to your server and update the UI if necessary
          console.log('Token received:', currentToken);
          messaging.subscribeToTopic(currentToken, topic)
            .then(() => {
              console.log('Subscribed to topic:', topic);
            })
            .catch((error) => {
              console.error('Error subscribing to topic:', error);
            });
        } else {
          // Show permission request UI
          console.log('No registration token available. Request permission to generate one.');
        }
      }).catch((err) => {
        console.error('An error occurred while retrieving token:', err);
      });
    }
  });

  onBackgroundMessage((payload) => {
    console.log('[firebase-messaging-sw.js] Received background message:', payload);
    // Customize notification here
    const notificationTitle = 'Background Message Title';
    const notificationOptions = {
      body: eventDetails,
      icon: '/firebase-logo.png'
    };

    self.registration.showNotification(notificationTitle, notificationOptions);
  });
}

// Listen for custom messages from Shiny
Shiny.addCustomMessageHandler("custom-message", function(message) {
  if (message) {
    requestPermissionAndSubscribeToTopic(message.topic, message.eventDetails);
  }
});

