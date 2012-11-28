var isVisible = false;
var clickedAway = false;

$(document).ready(function() {
  $('.filter-severity').click(function(e) {
    e.preventDefault();
  });

  $('.icon-pause').click(function(e) {
    var data = 'stream_id=' + encodeURIComponent(streamId);
    $.ajax({type:'POST',url:'/log/stream/pause',data:data,
            success:function(data,textStatus,xhr) {
              if (data['is_paused']) {
                $('#log-pause').removeClass('icon-pause').addClass('icon-play');
                $('#log-messages tbody').prepend($('<tr />')
                                                 .append($('<td />').attr('colspan', '4').css('text-align', 'center')
                                                         .html('Paused...')));
              } else {
                $('#log-pause').removeClass('icon-play').addClass('icon-pause');
              }
            },
            error:function(xhr,textStatus) {
              alert('Unable to toggle pause state');
            }});
    e.preventDefault();
  });
  $('.close-popover').live('click', function() {
    $('.show-more').popover('hide');
  });

  $('.show-more').popover({html: true, trigger: 'manual'})
                 .click(function(e) {
                     $('.show-more').popover('hide');
                     $(this).popover('show');
                     e.preventDefault();
                 });

  // select the default filters
  for (appliedFilter in appliedFilters) {
    var values = appliedFilters[appliedFilter];

    if (appliedFilter === 'node_names') {
      for (var i = 0; i < values.length; i++) {
        var value = values[i];
        $('.filter-node[data-val=\''+value+'\']').prop('checked', true);
      }
    }
  }

  showNewLogMessage = function(log_message) {
    var row = $('<tr />');
    var cell = $('<td />').css('padding-right', '12px');
    var more = $('<a />').attr('href', '#')
                          .attr('rel', 'popover')
                          .attr('data-placement', 'bottom')
                          .attr('data-html', true)
                          .attr('data-content', log_message.find_more_html)
                          .attr('data-original-title', 'Find Similar<div style="float:right;"><button class="close close-popover">&times;</button></div>')
                          .addClass('btn').addClass('btn-mini').addClass('show-more')
                          .html('...');
    more.popover({html: true, trigger: 'manual'})
                .click(function(e) {
                  $('.show-more').popover('hide');
                  $(this).popover('show');
                  e.preventDefault();
                });
    cell.append(more);
    row.append(cell);
    row.append($('<td />').html(log_message.time));
    row.append($('<td />').html(log_message.message_severity));
    row.append($('<td />').html(log_message.message));

    $('#log-messages tbody').prepend(row);

    // truncate the table to 500 rows // TODO make this less static
    while ($('#log-messages tr').length > 500) {
      $('#log-messages tr:last').remove();
    }
  }
});


