#### On proper introductions.

Hello there.

As documentation is usually written for humans, it's often useful to introduce concepts with a few words.

#### This title is below the last

You'll also note that multiple intros are possible.

## POST /greet

#### Request Body:

``` javascript
"Hello, haskeller!"
```

#### Response:

 - Status code 201
 - If you use ?capital=true

``` javascript
"HELLO, HASKELLER"
```

 - If you use ?capital=false

``` javascript
"Hello, haskeller"
```

## GET /hello;lang=<value>/:name

#### Captures:

- *name*: name of the person to greet

#### Matrix Parameters**:

**hello**:

 - lang
     - **Values**: *en, sv, fr*
     - **Description**: Get the greeting message selected language. Default is en.




#### GET Parameters:

 - capital
     - **Values**: *true, false*
     - **Description**: Get the greeting message in uppercase (true) or not (false).Default is false.


#### Response:

 - Status code 200
 - If you use ?capital=true

``` javascript
"HELLO, HASKELLER"
```

 - If you use ?capital=false

``` javascript
"Hello, haskeller"
```

## DELETE /greet/:greetid

#### Captures:

- *greetid*: identifier of the greet msg to remove

#### Response:

 - Status code 204
 - No response body


