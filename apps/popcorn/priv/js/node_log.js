
showNewLogMessage = function(log_message) {
  var row = $('<tr />');
  row.append($('<td />').html(log_message.timestamp));
  row.append($('<td />').html(log_message.severity));
  row.append($('<td />').html(log_message.message));
  $('#log-messages tbody tr:first').before(row);
};
