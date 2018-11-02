# tasty-req

`tasty-req` is a testing library for HTTP services which accept and return JSON
bodies.  A basic example is

    1: POST /
    { "testing": 123 }
    ---
    { "result": "OK" }

This makes a POST request to the `/` path of the server we are testing against,
passing `{ "testing: 123 }` as the request body, and expecting to receive `{
"result": "OK" }` as the response body.

The runner currently supports only GET, POST, PUT, and DELETE methods.


## Type checking

Often we won't know the exact values to expect from a response, but we know it
will be of a certain type, such as an integer or a string.  We can express that
using type patterns, for example:

    1: POST /
    { "testing": 123 }
    ---
    { "id": {:t integer}, "testing": 123 }

This says that the object we expect to receive in response will have an integer
in its "id" field, which specific integer doesn't matter.  Type checks may only
be used on the response side of the exchange.

The possible types recognised are:

 - `string`
 - `integer`
 - `double`
 - `boolean`
 - `maybe <type>`

where `<type>` is one of the non-`maybe` types, and means that the value may
also be `null`.

## Back references

Sending representations of resources back and forth means a lot of repetition,
which can become onerous and error-prone to write out explicitly in a script.
We can address this difficulty using references to previously sent or received
data.

    1: POST /
    { "foo": 1, "bar": 2}
    ---
    { "id": {:t integer}, "object": {:r 1?} }

This expects as a response an object with an "id" field and an "object" field,
containing the same object as what we sent in the request (`?`) of exchange #1.
If we wanted to reference a response we can use the `!` character instead of
`?` in the reference.

Sometimes it is not the entire request or response that we want to refer to, but
only a sub-component of it.  An example might be an resource ID, or a URI link
that the server gives us in response to a POST to create a new resource.  To
handle this we can use paths in references.

    1: POST /
    { "testing": 123 }
    ---
    { "id": {:t integer}, "testing": 123 }

    2: PUT /{:r 1!["id"]}
    { "id": {:r 1!["id"]}, "testing": 456 }
    ---
    {:r 2!}

Now we make a follow up PUT to the resource created from the POST, using the
"id" field from the response to construct the URL, as well as supplying the id
and a new value for "testing".  We expect back the same response as the request
body.


## Merging JSON objects

Another case where references are useful is when the data sent and received is
repetitive, sending the same object structure in several calls.  These values
might share a large amount of structure, but a response might contain some extra
data, for example an "id" field for a newly created resource.  To express this
well, JSON object patterns can be merged with the `<>` operator, so that the
resultant object contains every field in the left operand, after resolving
references, and then having the fields in the right operand added, overriding
any values in the left operand that are common between the two.

    1: POST /
    { ... some large structure ... }
    ---
    {:r 1?} <> { "id": {:t integer } }

    2: GET /{:r 1!["id"] }
    ---
    {:r 2!}

This script creates a new resource with a POST request.  The response is
expected to be the same as the request, plus an "id" integer field.  The
subsequent GET request to the given resource expects to receive the same
response as to the POST request.


## Reference Omissions

Sometimes we'd like to make reference to some large object, but leaving out some
fields.  To do this we can use a 'slash' syntax in references.

    1: POST /
    { ... some large structure ... }
    ---
    {:r 1? / "foo" "bar" } <> { "id": {:t integer } }

This says we expect to receive back everything sent, except the "foo" and "bar"
fields, and with a new "id" field added in.


## Random generation

Sometimes we don't care to specify some parts of a structure, or we'd like them
to vary from one run to the next.  To express this there is some basic support
for randomly generating data.

    1: POST /
    { "name": {:g string},
      "size": {:g integer},
      "description": {:g maybe string}
    }
    ---
    {:r 1?}

Random generators may only be used on the request side of an exchange.


## Arrays

Arrays may appear anywhere in a JSON structure, including at the top level, and
may be referenced in the same way as objects.

    1: POST /
    [ { "name": "foo" }, { "name": "bar" } ]
    ---
    {:r 1?}

References to arrays can also index into them as with objects.

    1: POST /
    [ { "name": "foo" }, { "name": "bar" } ]
    ---
    [ {:r 1?[1]}, {:r 1?[0]} ]

This exchange expects the server to reverse the order of the items in the array.

The slash notation is not currently valid for arrays, though it could be used to
filter out items neatly.
