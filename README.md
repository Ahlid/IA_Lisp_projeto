## IA_Lisp_projeto

# Boards

A)
o   o   o   o
                
o   o   o---o
    |       |
o   o---o---o
        |   |
o   o   o---o

B)
o   o   o---o   o
        |   |       
o---o---o---o---o
        |       |
o   o   o---o---o
|   |   |   |   |
o   o   o---o---o
|   |   |   |   |
o   o   o---o---o

C)
o   o   o---o   o
            |       
o---o   o---o---o
                |
o   o   o---o---o
|   |   |   |   |
o   o   o---o---o
|   |   |   |   |
o   o   o---o---o

D)
o   o   o   o   o   o
                        
o   o   o   o   o   o
                        
o   o   o   o   o   o
                        
o   o   o   o   o   o
                        
o   o   o   o   o   o

E)
o   o   o   o---o   o   o
                |           
o   o   o   o---o---o---o
    |   |               |
o---o---o---o---o---o   o
        |   |   |   |   |
o   o   o   o---o---o   o
|           |       |   |
o   o   o   o---o---o   o
|   |   |       |       |
o   o   o---o---o---o---o
|   |   |               |
o   o   o---o---o---o---o

F)

o   o   o   o   o   o   o   o
                                
o   o   o   o   o   o   o   o
                                
o   o   o   o   o   o   o   o
                                
o   o   o   o   o   o   o   o
                                
o   o---o   o   o   o   o   o
    |                           
o   o---o   o   o   o   o   o
                                
o   o   o   o   o   o   o   o
                                
o   o   o   o   o   o   o   o


# Algorithms

|Algorithm	|Complexity|
| ------------- |:-------------:| 
|Breadth-First Search |	O(número de nós)|
|Depth-First Search	|O(número de nós)|
| A* Search	|O(N)|
| IDA* Search|	O(N2)|


Breadth-First Search
Breadth-First search is like crossing a tree where each one is a state that can be a potential candidate for a solution. It expands nodes from the root of the tree and then generates one level of the tree at a time to find a solution. At each iteration, the head of the queue is removed and then expanded. Our raised children are followed in tail queue.
Advantages:
- If there is a solution, BFS will definitely find out.
- If there is more than one solution, then BFS can find the minimum that requires the least number of steps.
Disadvantages:
- It needs lots of memory.
- If you have a very long answer.


Depth-First Search
Depth-First search is how to traverse each branch a tree where each is a state that may be a potential candidate for a solution. At each iteration, the head of the queue is removed and then expanded. Our children were added to the head of the queue.
Advantages:
- Memory requirement is more linear compared to the search graph
- You are in a solution without having to explore the landscape a lot (with great depth values), requiring little time and space.
Disadvantages:
- Does not guarantee that you will find a lower cost solution

A research
(N) = g (n) + h (n) where g (n) is the cost of its heuristic value. At each iteration, the head of the queue is removed and then expanded. Our children are also managed in addition to filament care.
Advantages:
- A path to a goal is a heuristic for admissible
- Expands as few knots as possible
Disadvantages:
- Requires lots of space
- Require an additional calculation to calculate h *

IDA * Search
It consists of applying a number of times the depth search method with variable depth limits, where the limit is given in terms of f. (N0) = g (n0) + h '(n0) = h' (n0), where n0 is the initial node. It only expands nodes with (n) <= L. If the solution is not found, we will use a new threshold such that L1 = min (f (n)) where F (n) is the set of nodes visited by expand.
Advantages:
- Memory requirement is more linear
- A path to a goal is a heuristic for admissible
Disadvantages:
- In this case, being a solution given by a sequence of Our Objectives (N2).

Comparative study
In this part of the document is a comparative study between the algorithms used in the resolution of the trays of the utterance.

![alt tag](http://image.prntscr.com/image/ed3e4a971f484eb58bb15a519997f896.png)


# Heuristic given

The given heuristic consists of h (x) = o (x) - c (x) - 1 (1) where:
- o (x) is the goal for this board: the number of boxes to close on the board x,
- c (x) is the number of boxes already closed in tray x.
This heuristic turns out to be permissible since h * <= h, however, it takes negative values ​​thus affecting the algorithm A * in an undesirable way and may even deceive it.

# Heuristic developed

The developed heuristic took several paths, it began to be a heuristic that was thought admissible, but, after tests of the group was found a case where it was not admissible. Due to not bring great benefit was decided to create a new based on the old, this consists of:
(No boxes to use with 1 bow for missing +
(No boxes to use with 2 bows for missing * 2) +
(No boxes to use with 3 bows for missing * 3) +
(No. of boxes to use with 4 bows for missing * 4)) - 1

This heuristic is not permissible, however this allows to verify the content learned in class. In class it was said that an inadmissible heuristic tends to find a solution more quickly, as can be seen in the statistics collected. In some cases it still takes admissibility due to the de-increment of 1 that arises due to two boxes with 1 bow for missing which is shareable. Thus, although this heuristic was not acceptable, it was a good study object having solved all the trays of the statement with A * and IDA *.

# Limitations of the Program

The program needs a lisp compiler to compile. In this case LispWorks was used to compile the program. This presents stack and heap limitations, so the program is only limited to resolving trays that do not exceed the features available in lispworks. Therefore, it is impossible to determine some statistics with some algorithms for certain trays.
