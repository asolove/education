open Core.Std;

type graph = {
  edges: Int.Map.t((int, Int.Map.t(bool)))
};

let show_graph = (graph) => Printf.sprintf("Graph: (%i nodes)", Map.length(graph.edges));

let randomGraph = (size: int) => {
  let adjacencies: list((int, (int, Int.Map.t(bool)))) = 
    List.mapi(
      List.range(0, size),
      (i, _v) => {
        let adjacent = List.map(
          List.range(0, Random.int(size)),
          (_v) => {
            (Random.int(size), true)
          });

        let mapOfAdjacent = Int.Map.of_alist_reduce(adjacent, ~f=(a, b) => true);
        let count = Map.length(mapOfAdjacent);
        (i, (count, mapOfAdjacent))
      }
    );
  {
    edges: Int.Map.of_alist_exn(adjacencies)
  }
};

let degree = (g: graph, v: int) => {
  switch(Map.find(g.edges, v)) {
  | None => 0
  | Some((c, _es)) => c
  }
};

let removeVertex = (g: graph, v: int): graph => {
  let vertex = Map.find(g.edges, v);
  switch(vertex) {
  | None => g
  | Some((_c, _adjacent)) => {
    let edgesWithoutV = Map.remove(g.edges, v);
    let edges' = Map.map(edgesWithoutV, ((c, vertices)) => {
        switch(Map.find(vertices, v)){
        | Some(true) => (c-1, Map.remove(vertices, v))
        | _else => (c, vertices)
        }
      });
    {
      edges: edges'
    }
  }
  }
};

let rec naiveMIG = (g: graph, k: int): graph => {
  let deletable = Map.fold(
    g.edges,
    ~init=[],
    ~f=(~key, ~data as (c, _es), res) => c < k ? [key, ...res] : res
  );
  switch(deletable) {
  | [] => g
  | vs => {
    let g' = List.fold_left(vs, ~init=g, ~f=(g,v) => removeVertex(g, v));
    naiveMIG(g', k)
  }
  }
};

let g = randomGraph(2000);
print_string(show_graph(g));
print_newline();
print_string(show_graph(naiveMIG(g, 400)));
print_newline();
