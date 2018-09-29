## On proper introductions.

Hello there.

As documentation is usually written for humans, it's often useful to introduce concepts with a few words.

## This title is below the last

You'll also note that multiple intros are possible.

## POST /greet

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- If you use ?capital=true (`application/json;charset=utf-8`, `application/json`):

```javascript
"HELLO, HASKELLER"
```

- If you use ?capital=false (`application/json;charset=utf-8`, `application/json`):

```javascript
"Hello, haskeller"
```

### Response:

- Status code 200
- Headers: [("X-Example","1729")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- If you use ?capital=true (`application/json;charset=utf-8`, `application/json`):

```javascript
"HELLO, HASKELLER"
```

- If you use ?capital=false (`application/json;charset=utf-8`, `application/json`):

```javascript
"Hello, haskeller"
```

## DELETE /greet/:greetid

### Title

This is some text

### Second secton

And some more

### Captures:

- *greetid*: identifier of the greet msg to remove

### Headers:

- This endpoint is sensitive to the value of the **unicorns** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## GET /hello/:name

### Captures:

- *name*: name of the person to greet

### GET Parameters:

- capital
     - **Values**: *true, false*
     - **Description**: Get the greeting message in uppercase (true) or not (false).Default is false.


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `text/plain;charset=utf-8`

- If you use ?capital=true (`application/json;charset=utf-8`, `application/json`, `text/plain;charset=utf-8`):

```javascript
"HELLO, HASKELLER"
```

- If you use ?capital=false (`application/json;charset=utf-8`, `application/json`):

```javascript
"Hello, haskeller"
```
