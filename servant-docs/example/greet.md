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

### Sample Request:

```bash
curl -XPOST \
  -H "Content-Type: application/json;charset=utf-8" \
  -d "\"HELLO, HASKELLER\"" \
  http://localhost:80/greet
```

## DELETE /greet/:greetid

### Title

This is some text

### Second section

And some more

### Captures:

- *greetid*: identifier of the greet msg to remove

### Headers:

- This endpoint is sensitive to the value of the **X-Num-Unicorns** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

### Sample Request:

```bash
curl -XDELETE \
  -H "X-Num-Unicorns: 1" \
  http://localhost:80/greet/:greetid
```

## GET /hello/:name

### Captures:

- *name*: name of the person to greet

### Headers:

- This endpoint is sensitive to the value of the **X-Num-Fairies** HTTP header.

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

### Sample Request:

```bash
curl -XGET \
  -H "X-Num-Fairies: 1729" \
  http://localhost:80/hello/:name
```


