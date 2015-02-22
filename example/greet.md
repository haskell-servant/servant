#### On proper introductions.

Hello there.

As documentation is usually written for humans, it's often useful to introduce concepts with a few words.

#### This title is below the last

You'll also note that multiple intros are possible.

## POST /greet

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
"Hello, haskeller!"
```

#### Response:

- Status code 201

- Supported content types are:

    - `application/json`

- If you use ?capital=true

```javascript
"HELLO, HASKELLER"
```

- If you use ?capital=false

```javascript
"Hello, haskeller"
```

## GET /hello;lang=<value>/:name

#### Captures:

- *name*: name of the person to greet

#### Matrix Parameters:

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

- Supported content types are:

    - `application/json`
    - `text/plain;charset=utf-8`

- If you use ?capital=true

```javascript
"HELLO, HASKELLER"
```

- If you use ?capital=true

```
"HELLO, HASKELLER"
```

- If you use ?capital=false

```javascript
"Hello, haskeller"
```

- If you use ?capital=false

```
"Hello, haskeller"
```

## DELETE /greet/:greetid

#### Title

This is some text

#### Second secton

And some more

#### Captures:

- *greetid*: identifier of the greet msg to remove


- This endpoint is sensitive to the value of the **unicorns** HTTP header.

#### Response:

- Status code 200

- No response body


