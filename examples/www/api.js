
function increaseCounter(onSuccess, onError)
{
  $.ajax(
    { url: '/counter'
    , success: onSuccess 
    , error: onError
    , type: 'POST'
    });
}
  
function getCurrentValue(onSuccess, onError)
{
  $.ajax(
    { url: '/counter'
    , success: onSuccess 
    , error: onError
    , type: 'GET'
    });
}
  