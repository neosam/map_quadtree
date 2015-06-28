open Quadtree


type 'a pair = 'a * 'a

type 'a storage_tree = Storage of ('a list, 'a storage_tree) tree

type 'a pos_fn = 'a -> int pair

type 'a storage_map = ('a list, 'a storage_tree) map

type 'a storage = {
    map: 'a storage_map;
    pos_fn: 'a pos_fn
}



let rec create_storage_tree (depth: int): 'a storage_tree = match depth with

    | 0 -> Storage (Field [])
    | _ ->
            let (Storage sub_tree) = create_storage_tree (depth - 1) in
            let four_node_tuple = (sub_tree, sub_tree, sub_tree, sub_tree) in
            Storage (Node (four_node_tuple, Storage sub_tree))

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


let set_map (map: 'a storage_map) (storage: 'a storage): 'a storage = {
    storage with map = map
}


let storage_elements_at (pos: int pair) (storage: 'a storage): 'a list =
    Quadtree.field_at pos storage.map

(* TODO:  Optimize if list is empty *)
let storage_set_elements (pos: int pair)
                      (new_elements: 'a list)
                      (storage: 'a storage): 'a storage =
    set_map (Quadtree.set_field pos new_elements storage.map) storage

let storage_add (elem: 'a) (storage: 'a storage): 'a storage =
    let position = storage.pos_fn elem in
    storage_set_elements position
                         (elem :: (storage_elements_at position storage))
                         storage

let storage_remove (elem: 'a) (storage: 'a storage): 'a storage =
    let position = storage.pos_fn elem in
    storage_set_elements position
                         (List.filter (fun x -> x <> elem)
                                      (storage_elements_at position storage))
                         storage





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



