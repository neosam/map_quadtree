# Pure functional 2D map in OCaml

Since arrays are not available in function programming, there must be a
different way to build a 2D map.  This file implements a quadtree which
is used a map type to store and get data.

A quadtree is a try, where each node has exact four childs.  In this
case one for top_left, top_right, bottom_left and bottom_right.  If the
tree is deep enough it can match any map size.

Everything in the world has advantages and disadvantages.  The main
disadvantage of his tree is that the access for the fields takes
log(n) while a array field would take log(1).

One advantage of this tree is that it takes almost no memory after it
is initialized.  Only when data is filled, memory will grow.  You can
create a fully function map with a dimension of 1M x 1M (M = mega/million)
fields in a few miliseconds.  Do this with an array and you will produce
1TB memory if you only store bytes.  But as sayed, fill everything and
it will take twice as much as an array map.

Since we cannot store such an amount of data in the memory, the tree type
can also calculate the nodes using a function.  This can drill down the
tree without consuming any memory by generating slightly modified copies
of itself for the nodes.  At a defined level it can start a loader
a loader function which loads the subtree into the memory.  So this map
automatically supports persistence data storage.


# Usage

You can use the "create_node_map" command to create a new 2d map.
The syntax is
```
create_node_map (width, height) default_field
```

* (width, height) stands for the size of the map and
* default_field will be used to fill the map.  It will also be used if 
something is unexpected or out of bounds.

This is an example to create a 200x150 sized field of empty strings:
```ocaml
let map = create_node_map (200, 150) ""
```

This is pretty fast, you can also create an 1,000,000 x 1,000,000 field without
any problem:
```ocaml
let map = create_node_map (1000000, 1000000) ""
```

You can look at your memory and you will see no big change.  But it will
increase if the field are set with different values.

To get a field, the "field_at" function can be used:
```ocaml
field_at (23, 42) map
```
You will get an empty string as defined as default above.

With "set_field", you can replace a field in the map.  Please note the map
itself is immutable.  Instead of changing the map, the "set_field" function
will return a new map.  Try this out:

```ocaml
let map2 = set_field (23, 42) "foo" map;;
field_at (23, 42) map;;
field_at (23, 42) map2;;
```

You will get an empty string for the "field_at" call with map and "foo" for
the field_at call with map2.


# Data structure

Lets talk about more details of the data structure.  Basically it looks like
this:
```
       node
     / |  | \
    /  |  |  \
   /   /  \   \
node node node node
```

Each node stands for a square of fields and divides it into four quarters.
In general, these quarters also are nodes but in the end, they will be fields.
That's how the functions can recursively drill
down to the requested field.  

```
          node
        / |  | \
       /  |  |  \
      /   /  \   \
     /   /    \   \
field field field field
```

