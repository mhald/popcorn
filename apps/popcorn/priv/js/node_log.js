
showNewLogMessage = function(log_message) {
  var row = $('<tr />');
  row.append($('<td />').html(log_message.time));
  row.append($('<td />').html(log_message.severity));
  row.append($('<td />').html(log_message.message));
  $('#log-messages tbody tr:first').before(row);

  // truncate the table to 500 rows // TODO make this less static
  while ($('#log-messages tr').length > 500) {
    $('#log-messages tr:last').remove();
  }
};
