

struct Node: Hashable, CustomDebugStringConvertible {
  var name: String
  
  var debugDescription: String { "{\(name)}" }
}

struct Edge: Hashable, CustomDebugStringConvertible {
  var from: Node
  var to: Node

  var debugDescription: String { "{\(from.name) -> \(to.name)}" }
}

struct Degree: CustomDebugStringConvertible {
  var `in`: Int = 0
  var out : Int = 0
  
  var debugDescription: String { "{in: \(self.in), out: \(self.out)}"}
}

enum GraphError: Error {
  case nodeNotOnGraph
}

struct Graph: CustomDebugStringConvertible {
  // Graph is being represented as a adjacency list.
  private var adjacentList: [Node: Set<Node>] = [:]
  
  private(set) var nodes: [Node] = []
  
  var edges: [Edge] {
    adjacentList.flatMap { (entry) -> [Edge] in
      entry.value.map { Edge(from: entry.key, to: $0) }
    }.sorted { $0.from.name < $1.to.name }
  }
  
  init() {}
  
  init(grid: [[String]]) {
    func findAdjacents(grid: [[String]], i: Int, j: Int) -> [String] {
      var adjecents: [String] = []
      if i - 1 >= 0 {
        adjecents.append(grid[i - 1][j])
      }
      
      if i + 1 < grid.count {
        adjecents.append(grid[i + 1][j])
      }
      
      if j - 1 >= 0 {
        adjecents.append(grid[i][j - 1])
      }
      
      if j + 1 < grid[i].count {
        adjecents.append(grid[i][j + 1])
      }
      
      return adjecents
    }
    
    for i in 0..<grid.count {
      for j in 0..<grid[i].count {
        let n = Node(name: grid[i][j])
        if !contains(node: n) {
          add(node: n)
        }
        for a in findAdjacents(grid: grid, i: i, j: j) {
          let an = Node(name: a)
          if !contains(node: an) {
            add(node: an)
          }
          try! addDirectEdge(from: n, to: an)
        }
      }
    }
  }
  
  func contains(node: Node) -> Bool { adjacentList[node] != nil }
  
  mutating func add(node: Node) {
    guard !contains(node: node) else { return }
    nodes.append(node)
    adjacentList[node] = []
  }
  
  mutating func add<S: Sequence>(nodes: S) where S.Element == Node {
    nodes.forEach { add(node: $0) }
  }
  
  @discardableResult
  mutating func addDirectEdge(from: Node, to: Node) throws -> Bool {
    guard contains(node: from) && contains(node: to) else {
      throw GraphError.nodeNotOnGraph
    }
    
    return adjacentList[from]!.insert(to).inserted
  }
  
  func dfs(node: Node, each: (Node) throws -> Void) throws {
    var visited: Set<Node> = []
    try dfsImpl(node: node, visited: &visited, each: each)
  }
  
  private func dfsImpl(node: Node,  visited: inout Set<Node>, each: (Node) throws -> Void) throws {
    guard contains(node: node) else {
      throw GraphError.nodeNotOnGraph
    }
    
    guard !visited.contains(node) else { return }
    
    try each(node)
    visited.insert(node)
    guard let adjacents = adjacentList[node], !adjacents.isEmpty else { return }
    
    for adj in adjacents {
      try dfsImpl(node: adj, visited: &visited, each: each)
    }
  }
  
  func bfs(node: Node, each: (Node)-> Void) throws {
    try bfsImpl(node: node, each: each)
  }
  
  func bridges(root: Node) throws -> [Edge] {
    var idxs: [Node: Int] = [:]
    var lowLinked: [Node: Int] = [:]
    var visited: Set<Node> = []
    return bridgesImpl(root: root, idx: 0, indexes: &idxs,
                       lowLinked: &lowLinked, visited: &visited)
  }
  
  private func bridgesImpl(root: Node,
                           idx: Int,
                           indexes: inout [Node: Int],
                           lowLinked: inout [Node: Int],
                           visited: inout Set<Node>) -> [Edge] {
    var bridges: [Edge] = []
    
    visited.insert(root)
    indexes[root] = idx
    lowLinked[root] = indexes[root]
    
    guard let adjacents = adjacentList[root], !adjacents.isEmpty else { return [] }
    for node in adjacents {
      guard node != root else {
        continue
      }
      
      if !visited.contains(node) {
        bridges += bridgesImpl(root: node, idx: idx + 1,
                               indexes: &indexes, lowLinked: &lowLinked,
                               visited: &visited)
        lowLinked[root] = min(lowLinked[root]!, lowLinked[node]!)
        if (indexes[root]! < lowLinked[node]!) {
          bridges.append(Edge(from: root, to: node))
        }
      } else {
        lowLinked[root] = min(lowLinked[root]!, lowLinked[node]! )
      }
    }
    
    return bridges
  }
    
  private func bfsImpl(node: Node, each: (Node)-> Void)  throws {
    guard contains(node: node) else {
      throw GraphError.nodeNotOnGraph
    }
    
    var queue: [Node] = [node]
    var visited: Set<Node> = []
    
    while !queue.isEmpty {
      let element = queue.removeFirst()
      if visited.contains(element) {
        continue
      }
      
      let adjacents = adjacentList[element] ?? []
      queue.append(contentsOf: adjacents)
      
      each(element)
      visited.insert(element)
    }
  }
  
  func isLeaf(node: Node) throws -> Bool {
    guard contains(node: node) else {
      throw GraphError.nodeNotOnGraph
    }
    return (adjacentList[node] ?? []).isEmpty
  }
  
  var debugDescription: String { adjacentList.debugDescription }
  
  func dump() {
    print("Nodes: \(nodes)")
    print("Edges: \(edges)")
  }
  
}

extension Graph {
  func countInOutDegrees() -> [Node: Degree] {
    var degrees: [Node: Degree] = [:]
    for edge in edges {
      // From
      var degree = degrees[edge.from, default: Degree()]
      degree.out += 1
      degrees[edge.from] = degree
      // To
      degree = degrees[edge.to, default: Degree()]
      degree.in += 1
      degrees[edge.to] = degree
    }
    return degrees
  }
  
  func hasEulerianPath() -> Bool {
    let degrees = countInOutDegrees()
    var startNodes: Int = 0
    var endNodes: Int = 0
    
    for node in nodes {
      let degree = degrees[node]!
      if degree.out - degree.in > 1 || degree.in - degree.out > 1 {
        return false
      } else if degree.out - degree.in == 1 {
        startNodes += 1
      } else if degree.in - degree.out == 1 {
        endNodes += 1
      }
    }
    
    let allNodesEqualInOutDegree = startNodes == 0 && endNodes == 0
    // At most one vertice has OutDegree - InDegree == 1 and one vertice InDegree - OutDegree == 1
    let atMostOne = startNodes == 1 && endNodes == 1
    return allNodesEqualInOutDegree || atMostOne
  }
  
  func hasEulerianCircuit() -> Bool {
    let degrees = countInOutDegrees()
    for node in nodes {
      let degree = degrees[node]!
      if degree.in != degree.out {
        return false
      }
    }
    return true
  }
  
  /*
   If at most one vertice has out_degree - in_degree = 1 and at most one has a in_degree - out_degree = 1
   we choose the node that has an extra out edge. Other wise if all edges has equal in-out edges just choose
   any node, this case the first.
   */
  func findEulerianStart(degrees: [Node: Degree]) -> Node? {
    guard let start = degrees.first(where: { (pair) -> Bool in
      return pair.value.out - pair.value.in > 0
    })?.key else {
      return nodes.first
    }
    return start
  }
  
  func getEulerianPath() -> [Node] {
    let degrees = countInOutDegrees()
    var outs = degrees.mapValues { $0.out }
    guard let start = findEulerianStart(degrees: degrees) else { return [] }
    var path: [Node] = []
    var visited: Set<Edge> = []
    getEulerianPathImpl(node: start, outs: &outs, path: &path, visited: &visited)
    return path
  }
  
  private func getEulerianPathImpl(node: Node,
                                   outs: inout [Node: Int],
                                   path: inout [Node],
                                   visited: inout Set<Edge>) {
    defer {
      if (outs[node] == 0) {
        path.insert(node, at: 0)
      }
    }
    
    guard let adjacents = adjacentList[node], !adjacents.isEmpty else {
      return
    }

    for child in adjacents {
      let edge = Edge(from: node, to: child)
      if visited.contains(edge) {
        continue
      }
      visited.insert(edge)
      
      getEulerianPathImpl(node: child, outs: &outs, path: &path, visited: &visited)
      outs[node]! -= 1
    }
  }
}

// Tree
struct BinaryTreeNode<E: Comparable> {
  var element: E
  var count: Int = 1
  private var childs: [BinaryTreeNode<E>?] = [nil, nil]
  
  var left: BinaryTreeNode<E>? {
    _read { yield childs[0] }
    set { childs[0] = newValue }
  }
  
  var right: BinaryTreeNode<E>? {
    _read { yield childs[1] }
    set { childs[1] = newValue }
  }
  
  init(element: E) {
    self.element = element
  }
}


struct BinaryTree<E: Comparable> {
  private var root: BinaryTreeNode<E>
  
  init(rootElement root: E) {
    self.root = BinaryTreeNode(element: root)
  }
  
  mutating func add(elements: E ...) {
    elements.forEach { add(element: $0) }
  }
  
  mutating func add(element: E) {
    add(node: BinaryTreeNode(element: element))
  }

  private mutating func add(node: BinaryTreeNode<E>) {
    addImpl(root: &root, node: node)
  }

  private func addImpl(root: inout BinaryTreeNode<E>, node: BinaryTreeNode<E>) {
    if (node.element == root.element) {
      root.count += 1
    } else {
      let childKp: WritableKeyPath<BinaryTreeNode<E>, BinaryTreeNode<E>?> = node.element < root.element ? \.left : \.right
      if var child = root[keyPath: childKp] {
        addImpl(root: &child, node: node)
        root[keyPath: childKp] = child
      } else {
        root[keyPath: childKp] = node
      }
    }
  }
  
  func traverse(_ each: (E) -> Void) {
    traverseImpl(root, each: each)
  }
  
  private func traverseImpl(_ root: BinaryTreeNode<E>, each: (E) -> Void) {
    for _ in 0..<root.count {
      each(root.element)
    }
    
    if let left = root.left {
      traverseImpl(left, each: each)
    }
    
    if let right = root.right {
      traverseImpl(right, each: each)
    }
  }
}


//==============================================================================================
// Functions

func graphTraversalDFS_BFS() throws {
  let n1 = Node(name: "1")
  let n2 = Node(name: "2")
  let n3 = Node(name: "3")
  let n4 = Node(name: "4")
  let n5 = Node(name: "5")

  var graph = Graph()
  graph.add(nodes: [n1, n2, n3, n4, n5])

  // Edges
  try graph.addDirectEdge(from: n1, to: n2)
  try graph.addDirectEdge(from: n2, to: n3)
  try graph.addDirectEdge(from: n2, to: n4)
  try graph.addDirectEdge(from: n4, to: n5)

  print("DFS Traversal")
  try graph.dfs(node: n1, each: { print($0) })

  print("BFS Traversal")
  try graph.bfs(node: n1, each: { print($0) })

  graph.dump()
}

func gridGraph() throws {
  let gridGraph = Graph(grid: [["0", "1"], ["2", "3"], ["4", "5"]])
  print("Grid Graph")

  //0 1
  //2 3
  //4 5
  try gridGraph.dfs(node: Node(name: "0"), each: { print($0) })
  gridGraph.dump()
}

func bridges() throws {
  let nodes = (0...8).map({ Node(name: "\($0)") })
  var graph = Graph()
  graph.add(nodes: nodes)
  
  try graph.addDirectEdge(from: nodes[0], to: nodes[1])
  try graph.addDirectEdge(from: nodes[1], to: nodes[2])
  try graph.addDirectEdge(from: nodes[2], to: nodes[0])
  try graph.addDirectEdge(from: nodes[2], to: nodes[3])
  try graph.addDirectEdge(from: nodes[3], to: nodes[4])
  try graph.addDirectEdge(from: nodes[3], to: nodes[4])
  try graph.addDirectEdge(from: nodes[2], to: nodes[5])
  try graph.addDirectEdge(from: nodes[5], to: nodes[6])
  try graph.addDirectEdge(from: nodes[6], to: nodes[7])
  try graph.addDirectEdge(from: nodes[7], to: nodes[8])
  try graph.addDirectEdge(from: nodes[8], to: nodes[5])

  print("Bridges")
  let briges = try graph.bridges(root: nodes[0])
  print(briges)
  
  graph.dump()
  print(graph.countInOutDegrees())
}

func binaryTreeTraversal() {
  var btree = BinaryTree<Int>(rootElement: 5)
  btree.add(elements: 3, 10, 11, 4, 3, 2 , 1, 8)

  print("Binary Tree")
  btree.traverse { print($0) }
}

func eulerianPath() throws {
  print("Eulerian Path")

  let nodes = (0...4).map({ Node(name: "\($0)") })
  var graph = Graph()
  graph.add(nodes: nodes)
  
  try graph.addDirectEdge(from: nodes[0], to: nodes[1])
  try graph.addDirectEdge(from: nodes[1], to: nodes[2])
  try graph.addDirectEdge(from: nodes[2], to: nodes[0])
  try graph.addDirectEdge(from: nodes[0], to: nodes[3])
  try graph.addDirectEdge(from: nodes[3], to: nodes[4])

  print("Has Eulerian Path: \(graph.hasEulerianPath())")
  print("Has Eulerian Circuit: \(graph.hasEulerianCircuit())")
  print("Eulerian path: \(graph.getEulerianPath())")

}

func eulerianCircuit() throws {
  print("Eulerian Circuit")
  
  let nodes = (0...4).map({ Node(name: "\($0)") })
  var graph = Graph()
  graph.add(nodes: nodes)
  
  try graph.addDirectEdge(from: nodes[0], to: nodes[1])
  try graph.addDirectEdge(from: nodes[1], to: nodes[2])
  try graph.addDirectEdge(from: nodes[2], to: nodes[0])
  try graph.addDirectEdge(from: nodes[0], to: nodes[3])
  try graph.addDirectEdge(from: nodes[3], to: nodes[4])
  try graph.addDirectEdge(from: nodes[4], to: nodes[0])


  print("Has Eulerian Path: \(graph.hasEulerianPath())")
  print("Has Eulerian Circuit: \(graph.hasEulerianCircuit())")
  print("Eulerian Circuit: \(graph.getEulerianPath())")

}

binaryTreeTraversal()

try graphTraversalDFS_BFS()

// Building a graph from a grid.
try gridGraph()

try bridges()

// Eulerian
try eulerianPath()
try eulerianCircuit()
