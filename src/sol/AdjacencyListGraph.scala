package sol

import src.{Graph, Node}

/**
 * Class for the adjacency list representation of a Graph.
 * @tparam T the type of the nodes' contents
 */
class AdjacencyListGraph[T] () extends Graph[T] {

 //graph field containing  list of nodes present in graph
 private var graph: List[Node[T]] = List()

  /**
   * An AdjacencyList implementation of a graph node
   * @param contents - the value stored at that node
   * @param getsTo - the list of nodes which this node gets to
   */
  class ALNode( private val contents: T,
                private var getsTo: List[ALNode]) extends Node[T] {


    /**
     * Adds an outgoing directional edge from this node to another node.
     *
     * @param toNode - the node that the current node connects to
     */
    @Override
    override def addEdge(toNode: Node[T]): Unit = {

      if (toNode.isInstanceOf[ALNode]) {
        getsTo = getsTo :+ toNode.asInstanceOf[ALNode]
      }

    }

    /**
     * Returns the contents at this node.
     *
     * @return contents of type T
     */
    @Override
    override def getContents(): T = {contents}

    /**
     * Finds all nodes connected to this node by outgoing edges
     *
     * @return a List[Node[T]] of all nodes that the current node connects to
     */
    @Override
    override def getNexts(): List[Node[T]] = {getsTo}

}




  /**
   * Adds a node to the graph with specified contents.
   * Does not create an edge automatically.
   *
   * @param contents - the contents inside the new node
   * @return the Node[T] added to the graph
   */
  @Override
  override def createNode(contents: T): Node[T] = {
    val newNode = new ALNode(contents,List[ALNode]() )
    graph = graph :+ newNode
    newNode

  }

  /**
   * Adds a directional (one way) edge that connects two nodes
   *
   * @param fromNode - the node that contains the edge
   * @param toNode   - the node that the edge points to
   */
  @Override
  override def addEdge(fromNode: Node[T], toNode: Node[T]): Unit = {
    fromNode.addEdge(toNode)
  }

  /**
   * Method which prints the graph.
   */
  @Override
  override def show(): Unit = {
    for (node <- graph) {
      val nodeContents = node.getContents()
      val NodeGetsTo = node.getNexts()
      System.out.println(nodeContents + ";"  + NodeGetsTo )
    }
  }

  /**
   * Helper method that returns private  graph field
   * @return
   */
  def getGraph(): List[Node[T]] = {graph}

  
}

object AdjacencyListGraph extends App {


}
