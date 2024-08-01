 document.getElementById('myForm').addEventListener('submit', function(event) {
    var password = document.getElementById('password_recover').value;
    var confirm = document.getElementById('password_confirm').value;

    if (password !== confirm) {
      event.preventDefault(); // Prevent the form submission
      alert('Passwords do not match');
      // You can also show the alert in a more stylish way, using a modal or an error message
    }
  });