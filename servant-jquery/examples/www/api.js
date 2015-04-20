
function postcounter(onSuccess, onError)
{
  $.ajax(
    { url: '/counter'
    , success: onSuccess
    , error: onError
    , type: 'POST'
    });
}

function getcounter(onSuccess, onError)
{
  $.ajax(
    { url: '/counter'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
