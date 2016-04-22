# Testing that servers behave identically

## Rewriting an application

If you are rewriting, or significantly refactoring, an application, you often
want to ensure that the behaviour of the rewritten application is the same as
that of the old one. Sometimes what the behaviour of the old application is is
not always clear, making the process a difficult and error-prone one.

**servant-quickcheck** can help. It provides a `serversEqual` function that,
given a **servant** API type and two URLs, generates arbitrary requests of the
right type and checks that, for the same request *history*, the two servers
respond identically.

To see how this works, let's re-implement the [Django
Todo-Backend](https://github.com/mihirk/todo-backend-django) application
in **servant**. (`serversEqual` works for non-**servant** applications, though
it's somewhat nicer to use when one of them is written with **servant**.) You
don't need to know anything about Django or Python to follow along; indeed,
part of the fun of it is using `serversEqual` to guide you through
re-implementing code you may not entirely understand.

Looking at the code, we can see the routes in `urls.py`:

``` python
urlpatterns = patterns('',
                       url(r'^$', RedirectView.as_view(url='/todos')),
                       url(r'^todos$', views.TodoList.as_view()),
                       url(r'^todo/(?P<pk>[0-9]+)$', views.Todo.as_view()),
)
```

And the handlers in `views.py`:

``` python
class TodoList(APIView):
    def get(self, request, format=None):
        todo_items = TodoItem.objects.all()
        serializer = TodoItemSerializer(todo_items, many=True)
        return JSONResponse(serializer.data, status=status.HTTP_200_OK)

    def post(self, request, format=None):
        serializer = TodoItemSerializer(data=request.DATA)
        if serializer.is_valid():
            saved_item = serializer.save()
            saved_item.url = request.build_absolute_uri('/todo/' + str(saved_item.id))
            saved_item.save()
            serializer = TodoItemSerializer(instance=saved_item)
            return JSONResponse(serializer.data, status=status.HTTP_201_CREATED)
        return JSONResponse(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

    def delete(self, request, format=None):
        TodoItem.objects.all().delete()
        return JSONResponse(None, status=status.HTTP_204_NO_CONTENT)

class Todo(APIView):
    def get(self, request, pk, format=None):
        try:
            todoItem = TodoItem.objects.get(pk=pk)
            serializer = TodoItemSerializer(todoItem)
        except TodoItem.DoesNotExist:
            return JSONResponse(None, status=status.HTTP_400_BAD_REQUEST)
        return JSONResponse(serializer.data, status=status.HTTP_200_OK)

    def delete(self, request, pk, format=None):
        try:
            todoItem = TodoItem.objects.get(pk=pk)
            todoItem.delete()
        except TodoItem.DoesNotExist:
            return JSONResponse(None, status=status.HTTP_400_BAD_REQUEST)
        return JSONResponse(None, status=status.HTTP_204_NO_CONTENT)

    def patch(self, request, pk, format=None):
        try:
            todoItem = TodoItem.objects.get(pk=pk)
        except TodoItem.DoesNotExist:
            return JSONResponse(None, status=status.HTTP_400_BAD_REQUEST)
        serializer = TodoItemSerializer(data=request.DATA, instance=todoItem, partial=True)
        if serializer.is_valid():
            serializer.save()
            return JSONResponse(serializer.data, status=status.HTTP_200_OK)
        return JSONResponse(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
```

And from `models.py`:

``` python

class TodoItem(models.Model):
    title = models.CharField(max_length=256, null=True, blank=True)
    completed = models.NullBooleanField(null=True, blank=True, default=False)
    url = models.CharField(max_length=256, null=True, blank=True)
    order = models.IntegerField(null=True, blank=True)

```

So as a first pass, let's try:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Servant
import Servant.QuickCheck
import STMContainers.Map as M
import GHC.Conc (atomically)
import Test.QuickCheck

data Todo = Todo
    { title     :: String
    , completed :: Bool
    , url       :: String
    , order     :: Int
    } deriving (Eq, Show, Read)

type API = TodosAPI :<|> TodoAPI

type TodosAPI
  =   "todos" :>
           ( Get '[JSON] [Todo]
       :<|>  ReqBody '[JSON] Todo :> Post '[JSON] ()
       :<|>  Delete '[JSON] ())

type TodoAPI
  =    "todo" :> Capture "id " Int :>
           ( Get '[JSON] Todo
       :<|>  ReqBody '[JSON] Todo :> Patch '[JSON] ()
       :<|> Delete '[JSON} ())

serverTodos :: Server TodosAPI
serverTodos tvar = getTodos tvar
              :<|> postTodos tvar
              :<|> deleteAllTodos tvar

serverTodo :: Server TodoAPI
serverTodo id' = getTodo tvar id'
            :<|> patchTodo tvar id'
            :<|> deleteTodo tvar id'

getTodos :: Map Int Todo -> Handler [Todo]
getTodos m = liftIO . atomically . toList $ S.stream m

postTodos :: Map Int Todo -> Todo -> Handler ()
postTodos m t = liftIO . atomically $ S.insert m t

deleteTodos :: Map Int Todo -> Todo -> Handler ()
deleteTodos m t = liftIO . atomically $ S.insert m t
```

(We're keeping the `Todo`s in memory for simplicity - if this were a production
 application, we'd likely want to use a database.)

Notice that we split up the API into two sub-APIs. Partly this makes things
cleaner and more readable, but there's also a more concrete benefit: we can
start testing that **parts** of the API have been correctly rewritten without
implementing the entire server.

In order to check how we're doing, we need to add an `Arbitrary` instance for
`Todo`:

``` haskell
instance Arbitrary Todo where
  arbitrary = Todo <$> arbitrary <$> arbitrary <$> arbitrary <$> arbitrary
```


