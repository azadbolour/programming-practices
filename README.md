# Programming Practices

Code snippets and accompanying WIKI used to study programming concepts and practices.

* [Scala](scala/README.md)
* [Haskell](haskell/README.md)

Try pretty-printing sample code:

```scala
/**
 * Generic base trait for breadth-first search.
 */
trait BreadthFirstSearch[State] {

  def legalMoves(state: State): List[Move]

  /**
   * A move encapsulates a state change function from a current state to a new state.
   */
  trait Move {
    def change(state: State): State
  }

  /**
   * A sequence of successive moves taking us from the initial state to an final end point.
   *
   * @param moves: List of moves in this path in reverse order (for efficiency).
   * @param startState: The state at which this path started.
   * @param endState: The final state reached by traversing this path (cached for efficiency).
   */
  class Path(val moves: List[Move], val startState: State, val endState: State) {

    /**
     * Extend the path by a single move from its point.
     */
    def extend(move: Move) = new Path(move :: moves, startState, move change endState)
    
    // Utility functions.
    
    /**
     * Does path end in a given set of states? Shorthand for: set contains path.endState.
     */
    def endsIn(set: Set[State]) = set contains endState
    
    /**
     * For string display get the moves in the right order.
     */
    override def toString() = startState + ": " + (moves.reverse mkString " ") + " -> " + endState

    /**
     * Walk the path yielding the list of traversed states from startState to endState.
     */
    def walk(): List[State] = {
      val steps = moves.foldRight(List(startState)) {
        (move, states) => move.change(states.head) :: states
      }
      steps.reverse
    }
    
    /**
     * Walk the path mapping each traversed state.
     */
    def walk[A](stateMapper: State => A): List[A] = walk() map stateMapper
  }

  def endPoints(paths: Set[Path]) = paths map (_.endState)

  /**
   * The complete state of a breadth-first exploration at a given step.
   * Each step extends every leaf path with every legal move from its end point.
   *
   * @param paths All paths to all visited nodes so far.
   * @param frontierPaths Paths to end points that had not been visited before, that is,
   * paths to the leaf states in the tree of states reached by legal moves.
   * @param visited States visited so far, that is, the set of end points of all paths.
   */
  class Exploration(val paths: Set[Path], val frontierPaths: Set[Path], val visited: Set[State]) {

    def step(): Option[Exploration] = {
      if (frontierPaths.isEmpty) None
      else {
        val extensions = for {
          path <- frontierPaths
          extension <- legalMoves(path.endState) map path.extend if !(extension endsIn visited)
        } yield (extension)
        Some(new Exploration(paths ++ extensions, extensions, visited ++ endPoints(extensions)))
      }
    }
    
    // @model: stream cons
    
    /* 
     * Stream cons, that is, #::, is implemented in implicit consWrapper(stream: Stream), 
     * which takes the tail as by-name parameter, so will be evaluated on-demand.
     */

    // TODO. Use map and Scalaz ~ function to avoid match on option.
    def explore(): Stream[Exploration] = this #:: (
      step match {
        case None => Stream.empty
        case Some(x) => x.explore
      })
  }

  def startPath(start: State) = new Path(Nil, start, start)

  /**
   * For now just used for testing.
   * Maybe needed when we get to infinite state spaces.
   */
  def explorations(start: State) =
    (new Exploration(Set(startPath(start)), Set(startPath(start)), Set(start))).explore

  def explorationsToGoal(start: State, goal: State => Boolean) =
    explorations(start) filter ((x: Exploration) => x.paths.exists(goal apply _.endState))

  def solutions(explorationStream: Stream[Exploration], goal: State => Boolean) =
    (explorationStream map (_.paths) flatten) filter (goal apply _.endState)
    // (explorationStream map (_.paths) flatten) filter ((p: Path) => goal(p.endState))

  def finiteStateSpaceSolution(start: State, goal: State => Boolean) =
    solutions(explorations(start), goal) headOption

  def infiniteStateSpaceSolution(start: State, goal: State => Boolean, steps: Int) =
    solutions(explorations(start).take(steps + 1), goal) headOption

}

```
