open Quadtree

(** Pair type *)
type 'a pair = 'a * 'a

(** Special version of a tree which supports a storage *)
type 'a storage_tree = Storage of ('a list, 'a storage_tree) tree

(** Generate a postition from a type *)
type 'a pos_fn = 'a -> int pair

(** Special version of a map which stores a storage_tree *)
type 'a storage_map = ('a list, 'a storage_tree) map

(** Storage type is the main respresentive type for the storage functions *)
type 'a storage = {
    (* Stores the actual data *)
    map: 'a storage_map;

    (* Extracts the map position from 'a *)
    pos_fn: 'a pos_fn
}


(** Create the storage tree.
 * It's like the node_tree but it also stores the default structure in
 * the node to realize a memory save delete function *)
let rec create_storage_tree (depth: int): 'a storage_tree = match depth with

    | 0 -> Storage (Field [])
    | _ ->
            let (Storage sub_tree) = create_storage_tree (depth - 1) in
            let four_node_tuple = (sub_tree, sub_tree, sub_tree, sub_tree) in
            Storage (Node (four_node_tuple, Storage sub_tree))

(** Create a non-persistence storage *)
let create_storage ((width, height): int pair) (pos_fn: 'a pos_fn): 'a storage =
    let depth = depth_of_size (max width height) in
    let (Storage tree) = create_storage_tree depth in
    let (map: 'a storage_map) = {
        tree = tree;
        depth = depth;
        size = (width, height);
        default_field = [];
        default_node = Storage (Field [])
    } in {
        map = map;
        pos_fn = pos_fn
    }


(* Returns a new storage where the map is replaced *)
let set_map (map: 'a storage_map) (storage: 'a storage): 'a storage = {
    storage with map = map
}


(** Get the elements list from the given position *)
let storage_elements_at (pos: int pair) (storage: 'a storage): 'a list =
    Quadtree.field_at pos storage.map

(**
 * Replace all elements on the given position with the given list.
 * TODO:  Optimize if list is empty *)
let storage_set_elements (pos: int pair)
                      (new_elements: 'a list)
                      (storage: 'a storage): 'a storage =
    set_map (Quadtree.set_field pos new_elements storage.map) storage

(* Add a new element in the storage *)
let storage_add (elem: 'a) (storage: 'a storage): 'a storage =
    let position = storage.pos_fn elem in
    storage_set_elements position
                         (elem :: (storage_elements_at position storage))
                         storage

(* Remove the given element from the storage *)
let storage_remove (elem: 'a) (storage: 'a storage): 'a storage =
    let position = storage.pos_fn elem in
    storage_set_elements position
                         (List.filter (fun x -> x <> elem)
                                      (storage_elements_at position storage))
                         storage




(**
 * Like create_persistence_map but instead this one is a persistence storage
 *)

let create_persistence_storage ((width, height): int pair)
                                (save_depth: int)
                                (prefix: string)
                                (pos_fn: 'a pos_fn): 'a storage =
    let node_tree_gen depth_to_go _ _ _ depth _:
                                            ('a list, 'a storage_tree) tree =
        let (Storage tree) = create_storage_tree (depth_to_go - depth) in 
        tree in
    let depth = Quadtree.depth_of_size (max width height) in
    let loader_depth = depth - save_depth in
    let (map: 'a storage_map) = Quadtree.create_dynamic_map (width, height)
                                          []
                                          (Storage (Field []))
                                          loader_depth
                                          (Quadtree.file_node_tree
                                                   (default_filename_fn prefix)
                                                   node_tree_gen
                                                   depth [] (Storage (Field [])))
                                          (fun (_, _) _ ->
                                              (Storage (Field []))) in {
        map = map;
        pos_fn = pos_fn
     }



