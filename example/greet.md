POST /greet
-----------

**Request Body**: 

``` javascript
{"msg":"Hello, haskeller!"}
```

**Response**: 

 - Status code 201
 - Response body as below.

``` javascript
{"msg":"Hello, haskeller!"}
```

GET /hello/:name
----------------

**Captures**: 

- *name*: name of the person to greet

**GET Parameters**: 

 - capital
     - **Values**: *true, false*
     - **Description**: Get the greeting message in uppercase (true) or not (false). Default is false.


**Response**: 

 - Status code 200
 - Response body as below.

``` javascript
{"msg":"Hello, haskeller!"}
```

DELETE /greet/:greetid
----------------------

**Captures**: 

- *greetid*: identifier of the greet msg to remove

**Response**: 

 - Status code 204
 - No response body
