const signUpButton = document.getElementById('signUpBtn');
const signInButton = document.getElementById('signInBtn');
const container = document.getElementById('container');

signUpButton.addEventListener('click', () => {
  container.classList.add("right-panel-active");
});

signInButton.addEventListener('click', () => {
  container.classList.remove("right-panel-active");
});

// Lining effect for singup
var current = null;
document.getElementById('username_signup').addEventListener('focus', function(e) {
  if (current) current.pause();
  current = anime({
    targets: '#path_signup',
    strokeDashoffset: {
      value: 0,
      duration: 700,
      easing: 'easeOutQuart'
    },
    strokeDasharray: {
      value: '240 1850',
      duration: 700,
      easing: 'easeOutQuart'
    }
  });
});


//* This part is for handle lining effect*//
document.getElementById('email_signup').addEventListener('focus', function(e) {
  if (current) current.pause();
  current = anime({
    targets: '#path_signup',
    strokeDashoffset: {
      value: -388,
      duration: 700,
      easing: 'easeOutQuart'
    },
    strokeDasharray: {
      value: '240 1850',
      duration: 700,
      easing: 'easeOutQuart'
    }
  });
});

document.getElementById('password_signup').addEventListener('focus', function(e) {
  if (current) current.pause();
  current = anime({
    targets: '#path_signup',
    strokeDashoffset: {
      value: -725,
      duration: 700,
      easing: 'easeOutQuart'
    },
    strokeDasharray: {
      value: '240 1850',
      duration: 700,
      easing: 'easeOutQuart'
    }
  });
});

document.querySelector('#submitSignUp').addEventListener('focus', function(e) {
  if (current) current.pause();
  current = anime({
    targets: '#path_signup',
    strokeDashoffset: {
      value: -1350,
      duration: 700,
      easing: 'easeOutQuart'
    },
    strokeDasharray: {
      value: '800 1850',
      duration: 700,
      easing: 'easeOutQuart'
    },
  });
});

// Lining effect for singin
document.getElementById('email_signin').addEventListener('focus', function(e) {
  if (current) current.pause();
  current = anime({
    targets: '#path_signin',
    strokeDashoffset: {
      value: 0,
      duration: 700,
      easing: 'easeOutQuart'
    },
    strokeDasharray: {
      value: '240 1850',
      duration: 700,
      easing: 'easeOutQuart'
    }
  });
});

document.getElementById('password_signin').addEventListener('focus', function(e) {
  if (current) current.pause();
  current = anime({
    targets: '#path_signin',
    strokeDashoffset: {
      value: -388,
      duration: 700,
      easing: 'easeOutQuart'
    },
    strokeDasharray: {
      value: '240 1850',
      duration: 700,
      easing: 'easeOutQuart'
    }
  });
});

document.querySelector('#submitSignIn').addEventListener('focus', function(e) {
if (current) current.pause();
  current = anime({
    targets: '#path_signin',
    strokeDashoffset: {
      value: -1350,
      duration: 700,
      easing: 'easeOutQuart'
    },
    strokeDasharray: {
      value: '850 1850',
      duration: 700,
      easing: 'easeOutQuart'
    },
  });
});

  $(document).ready(function() {
    $("#signUpForm").submit(function(e) {
      e.preventDefault(); // Prevent the form from submitting normally

      // Capture input values
      var username = $("#username_signup").val();
      var email = $("#email_signup").val();
      var password = $("#password_signup").val();

      // Send input values to the server-side R code
      Shiny.setInputValue("username", username);
      Shiny.setInputValue("email", email);
      Shiny.setInputValue("password", password);
    });
  });