$(document).ready(function() {
  $('#login-btn').click(function() {
    $.ajax({type:'POST',url:'/api/v1/login',data:$('#login-form').serialize(),
            success:function(data, textStatus, xhr) {
              window.location.href = '/';
            },
            error:function(xhr, textStatus) {
              alert('failed');
            }});
    return false;
  });
});
