(* 2D Map storage in OCaml using a quadtree.
 *
 * Since arrays are not available in function programming, there must be a
 * different way to build a 2D map.  This file implements a quadtree which
 * is used a map type to store and get data.
 *
 * A quadtree is a try, where each node has exact four childs.  In this
 * case one for top_left, top_right, bottom_left and bottom_right.  If the
 * tree is deep enough it can match any map size.
 *
 * Everything in the world has advantages and disadvantages.  The main
 * disadvantage of his tree is that the access for the fields takes
 * log(n) while a array field would take log(1).
 *
 * One advantage of this tree is that it takes almost no memory after it
 * is initialized.  Only when data is filled, memory will grow.  You can
 * create a fully function map with a dimension of 1M x 1M (M = mega/million)
 * fields in a few miliseconds.  Do this with an array and you will produce
 * 1TB memory if you only store bytes.  But as sayed, fill everything and
 * it will take twice as much as an array map.
 *
 * Since we cannot store such an amount of data in the memory, the tree type
 * can also calculate the nodes using a function.  This can drill down the
 * tree without consuming any memory by generating slightly modified copies
 * of itself for the nodes.  At a defined level it can start a loader
 * a loader function which loads the subtree into the memory.  So this map
 * automatically supports persistence data storage.
 *
 * Author: Simon Goller
 *)



(***** Type definitions *)

(**
 * Since this is a quadtree, we need a four tuple
 *)
type 'a four_tuple = 'a * 'a * 'a * 'a

(**
 * Lets define some kind of commands for the dynamic trees.
 *)
type command =
    | Read
    | Write

(**
 * Quadtree definition.
 *)
type 'a tree =
    (* It can have a field which is the leaf of the tree *)
    | Field of 'a

    (* It can have a node always has four childs *)
    | Node of 'a tree four_tuple

    (* It can have a generator function which produces a Field or a Node 
     * If it generates a new Functon, it also must be evaluated.  This
     * can lead in a infinity loop. *)
    | Function of (command -> 'a tree)

(**
 * To navigate while drilling down the tree, there are four directions.
 *)
type direction =
    | Top_left
    | Top_right
    | Bottom_left
    | Bottom_right

(**
 * The map type which covers some meta data along with the tree.
 *)
type 'a map = {
    (* The tree itself to store the data *)
    tree: 'a tree;

    (* The depth of the tree *)
    depth: int;

    (* The map size *)
    size: int * int;

    (* The default field which is used on failures like out of bounce. *)
    default_field: 'a
}






(****** Helper functions *)

(* Calculate the depth according to the map dimension.  Size is an integer
 * which represents the greater dimension  (max width height).
 *)
let depth_of_size size =
    let float_size = float_of_int size in
    let float_depth = (log float_size) /. (log 2.) in
    let depth = (int_of_float float_depth) + 1 in
    depth

(** I don't know the pow function so I implemented it myself *)
let rec pow x y =
    let even = (y mod 2) == 0 in
    if y == 0 then 1
    else if even then
        let tmp = pow x (y / 2) in
        tmp * tmp
    else x * (pow x (y - 1))



(****** Tree functions *)

(* Evaluate the lazy function if it is one.
 * Will return a Field or Node. *)
let rec resolve_fn command = function
    | Field field -> Field field
    | Node node -> Node node
    | Function fn -> resolve_fn command (fn command)

(* Dig down one level on a node *)
let dig (tl, tr, bl, br) command = function
    | Top_left -> resolve_fn command tl
    | Top_right -> resolve_fn command tr
    | Bottom_left -> resolve_fn command bl
    | Bottom_right -> resolve_fn command br

(* Replace a child of a node with "x" *)
let put (tl, tr, bl, br) x = function
    | Top_left -> (x, tr, bl, br)
    | Top_right -> (tl, x, bl, br)
    | Bottom_left -> (tl, tr, x, br)
    | Bottom_right -> (tl, tr, bl, x)


(* Create a tree which fields are all set to default_field. 
 * To save memory, it will create only one sub-node for each level but
 * will assign it to all four childs. *)
let create_node_tree init_depth default_field =
    let rec aux = function
        | 0 -> Field default_field
        | depth ->
                let new_node = aux (depth - 1) in
                Node (new_node, new_node, new_node, new_node) in
    aux init_depth

(** Get direction depending on the size and position
 * In a square which has the dimension size * size, on which quarter
 * of it is (x, y) located.
 *)
let get_direction (x, y) size =
    let half = size / 2 in
    if       x < half  && y <  half then    Top_left
    else if  x >= half && y <  half then    Top_right
    else if  x < half  && y >= half then    Bottom_left
    else                                    Bottom_right



(****** Map functions *)

(** Create a map which uses only nodes and no functions 
 * It will calculate the required depth from the given size, generate the
 * tree and returns it along with some other information. *)
let create_node_map (width, height) default_field =
    let depth = depth_of_size (min width height) in {
        tree = create_node_tree depth default_field;
        size = (width, height);
        depth = depth;
        default_field = default_field
    }

(** Set a new tree on a map *)
let map_set_tree tree map = { map with tree = tree }

(** Return the field if the last attribute is a field.
 * If it's a node, it will return the default_field and if it's a function,
 * it will evaluate it.
 *)
let rec field_of_tree map command = function
    | Field field -> field
    | Node node -> map.default_field
    | Function fn -> field_of_tree map command
                            (resolve_fn command (Function fn))


(** Check whether (x, y) is in the map (true) or out of bounds (false). *)
let pos_in_map (x, y) map =
    let (width, height) = map.size in
    x >= 0 && x < width && y >= 0 && y < height

(** Returns the field in the map on the given position *)
let field_at pos map =
    (* Recursive sub function which digs down the tree until it discovers
     * a field.  Or if size gets 0 and then it returns the default_field.
     *)
    let rec aux (inner_x, inner_y) size = function
        (* We are done here *)
        | Field field -> field
        (* We have function, lets evaluate it and work with the result *)
        | Function fn ->
                aux (inner_x, inner_y) size (resolve_fn Read (Function fn))
        (* We have a node, dig down more *)
        | Node node ->
            (* We are done if size is 0 *)
            if size <= 0 then map.default_field
            else
                (* Create new position and size for the recursive call *)
                let new_pos = (inner_x mod 2, inner_y mod 2)
                and new_size = size / 2 in

                (* Find the quarter where we have to go *)
                let direction = get_direction (inner_x, inner_y) size in

                (* Lets go deeper *)
                let sub_tree = dig node Read direction in

                (* Continue with sub node *)
                aux new_pos new_size sub_tree in
    (* Check if pos is valid.  If not return the default_field. *)
    if pos_in_map pos map then
        aux pos (pow 2 map.depth) map.tree
    else
        map.default_field

(**
 * Replace the field on the given position.
 * This function will return a new map.
 *)
let set_field pos field map =
    (* Recursive function which replaces the field and all nodes which are
     * between the root and this field.  The new root will be returned. *)
    let rec aux (inner_x, inner_y) size = function
         (* If we are at the field then lets set it *)
         | Field f -> Field field

         (* No support for functions now *)
         | Function fn -> Function fn

         (* If there is a node, lets replace one of its childs *)
         | Node node ->
             (* Stop if size is 0.  Return the current node so nothing is
              * changed. *)
             if size <= 0 then Node node
             else
                (* Calculate now pos and size for recursion call *)
                let new_pos = (inner_x mod 2, inner_y mod 2)
                and new_size = size / 2 in

                (* Find out where to go next *)
                let direction = get_direction (inner_x, inner_y) size in

                (* Get the sub tree *)
                let sub_tree = dig node Write direction in

                (* Do recursive call on the sub tree *)
                let new_sub_tree = aux new_pos new_size sub_tree in

                (* and replace the subtree with its result *)
                Node (put node new_sub_tree direction) in
    (* Check if pos is valid.  If not, simply return the map so nothing is
     * changed. *)
    if pos_in_map pos map then
        (* Change the tree of the map with the new one *)
        map_set_tree (aux pos (pow 2 map.depth) map.tree) map
    else
        map



(****** Dynamic map functions *)

(* Default tree function.
 * It will automatically generate sub nodes if called.
 * While it creates its sub functions, it will also calculate their
 * positions and depth.
 * As soon as it hits a specific depth, it will call 'fn' and pass the
 * position and depth.  This for example can be a loader function which
 * loads data from the hard drive.
 *)
let rec tree_func (x, y) (current_depth: int) min_depth fn command =
    if current_depth <= min_depth then fn (x, y) current_depth command
    else let new_depth = current_depth - 1 in Node (
          Function (tree_func (x * 2 + 0, y * 2 + 0) (new_depth) min_depth fn),
          Function (tree_func (x * 2 + 1, y * 2 + 0) (new_depth) min_depth fn),
          Function (tree_func (x * 2 + 0, y * 2 + 1) (new_depth) min_depth fn),
          Function (tree_func (x * 2 + 1, y * 2 + 1) (new_depth) min_depth fn))

(* Shortcut for inner_tree_func required for initialization by settings
 * the initial values.  min_depth, fn and command is left. *)
let init_tree_func min_depth fn command =
    tree_func (0, 0) 0 min_depth fn command

(**
 * Create a dynamic map using function generators until it reaches
 * "loader_depth".  At loader_depth, fn is called and will generate
 * it's own tree objects.
 *)
let create_dynamic_map (width, height) (default_field: 'a) (loader_depth: int)
                                (fn: int * int -> int -> command -> 'a tree) =
    let depth = depth_of_size (max width height) in
    let tree = init_tree_func loader_depth fn in {
        tree = Function tree;
        size = (width, height);
        depth = depth;
        default_field = default_field
    }

(**** Some loaders *)

(** Returns a node tree with the given depth *)
let rec static_node_tree depth_to_go field (x, y) depth _ =
    create_node_tree (depth_to_go - depth) field

(** Returns the field immeditelly without any depth calculation *)
let create_static_field field (_, _) _ _ = Field field



