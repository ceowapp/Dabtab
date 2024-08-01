// Import the functions you need from the SDKs you need
import { initializeApp } from "firebase/app";
import { getAnalytics } from "firebase/analytics";

// TODO: Add SDKs for Firebase products that you want to use
// https://firebase.google.com/docs/web/setup#available-libraries

// Your web app's Firebase configuration
// For Firebase JS SDK v7.20.0 and later, measurementId is optional
const firebaseConfig = {
  apiKey: "AIzaSyDa_EuacvLjGlRzx18WRMcvz93T6Q9-9oo",
  authDomain: "dabtab-697e6.firebaseapp.com",
  projectId: "dabtab-697e6",
  storageBucket: "dabtab-697e6.appspot.com",
  messagingSenderId: "1069179811756",
  appId: "1:1069179811756:web:f42dd11e648056c2016c09",
  measurementId: "G-KNRR4Y3SXD"
};

// Initialize Firebase
const app = initializeApp(firebaseConfig);
const analytics = getAnalytics(app);



