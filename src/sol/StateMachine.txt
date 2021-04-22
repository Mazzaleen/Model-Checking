package sol

import src.Graph
import src.Node

// ------------- DO NOT CHANGE --------------------
import org.apache.commons.csv._
import java.io.{File, FileReader, IOException}
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.collection.mutable
// ------------------------------------------------

/**
 * Class of the State Machine.
 * @param stateGraph - an object which implements Graph stored inside a
 *                   stateMachine.
 */
class StateMachine(stateGraph: Graph[StateData]) {

  // field for the starting state of the state machine
  private var startState : Option[Node[StateData]] = None



  /**
   * Initializes a StateMachine from CSV files.
   * @param nodeCSV - the filename of the CSV containing node information
   * @param edgesCSV - the filename of the CSV containing edges information
   * @param startState - the starting state of the state machine (the id of the
   *                   node)
   */
  def initFromCSV(nodeCSV: String, edgesCSV: String, startState: String) {
    val format = CSVFormat.RFC4180.withHeader();

    // Parse nodeCSV
    val idMap = new mutable.HashMap[String, Node[StateData]]()
    try {
      val parser: CSVParser = new CSVParser(new FileReader(new File(nodeCSV)),
        format)
      val records = parser.getRecords().asScala.toList
      for (record <- records) {
        idMap(record.get(0)) =
          stateGraph.createNode(new
              StateData(record.get(1).split(";").toList))

        if(record.get(0).equals(startState)) {
          this.startState = Some(idMap(record.get(0)))
        }
      }
    }
    catch {
      case e: IOException => println("error reading " + nodeCSV)
    }




    // Parse edgeCSV
    try {
      val parser: CSVParser = new
          CSVParser(new FileReader(new File(edgesCSV)), format)
      val records = parser.getRecords().asScala.toList
      for (record <- records) {
        // TODO : Modify initCSV so that it raises an IOException if  edgesCSV
        //  tries to use a state that was not listed in the nodeCSV
        if (idMap.contains(record.get(0)) &&
          idMap.contains(record.get(1))) {
          stateGraph.addEdge(idMap(record.get(0)), idMap(record.get(1)))
        } else {
          throw new IOException("state not listed in nodeCSV")
        }
      }
    }
    catch {
      case e: IOException => println("error reading" + edgesCSV)
    }


  }


  /**
   * Checks whether the checkNode method returns true on every state that is
   * reachable from the start state
   * @param checkNode -- Node that we want to check if is reachable
   * @return none if method is true on all states, returns some with a node
   *         a method the method fails
   */
  def checkAlways(checkNode: (StateData => Boolean)):Option[Node[StateData]] = {
    var alreadyBeen : Set[Node[StateData]] = Set()

    def alwaysHelper(startNode:Node[StateData],
                     checkNode: (StateData => Boolean)):
                      Option[Node[StateData]] = {

      //If the check node does not have start node's contents, return some
      if (!checkNode(startNode.getContents())) {
        return Some(startNode)
      }

      alreadyBeen += startNode
      if (startNode.getNexts().nonEmpty) {
        for (n <- startNode.getNexts()) {
          if (!alreadyBeen.contains(n)) {
            val dataStored = alwaysHelper(n, checkNode)
            dataStored match {
              case Some(variable) => return dataStored
              case None => None
            }
          }
        }
      }
      None
    }

    alwaysHelper(startState.get,checkNode)
  }


  /**
   * Checks whether checks whether the checkNode function returns false
   * on every state that is reachable from the startState.
   *
   * @param checkNode -- Node that we want to check if is not reachable
   * @return none if method is true on all states, returns some with a node
   *         a method the method fails
   */
  def checkNever(checkNode: StateData => Boolean): Option[Node[StateData]] = {
    checkAlways(stateData => !checkNode(stateData))
  }

    /**
     * Sets the start node
     *
     * @param startNode -- Node we want to set as startNode
     */
    def setStart(startNode: Node[StateData]): Unit = {
      startState = Some(startNode)
    }
  }


