/* book search */
function updateResults(data)
{
  console.log(data);
  $('#results').html("");
  $('#query').text("\"" + data.query + "\"");
  for(var i = 0; i < data.results.length; i++)
  {
    $('#results').append(renderBook(data.results[i]));
  }
}

function renderBook(book)
{
  var li = '<li><strong>' + book.title + '</strong>, <i>'
         + book.author + '</i> - ' + book.year + '</li>';
  return li;
}

function searchBooks()
{
  var q = $('#q').val();
  getBooks(q, updateResults, console.log)
}

searchBooks();
$('#q').keyup(function() {
  searchBooks();
});

/* approximating pi */
var count = 0;
var successes = 0;

function f(data)
{
  var x = data.x, y = data.y;
  if(x*x + y*y <= 1)
  {
    successes++;
  }

  count++;

  update('#count', count);
  update('#successes', successes);
  update('#pi', 4*successes/count);
}

function update(id, val)
{
  $(id).text(val);
}

function refresh()
{
  getPoint(f, console.log);
}

window.setInterval(refresh, 200);
