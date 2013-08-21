(* \chaptertitle{Undo}{General and Selective Recovery} *)

(* This module provides two facilities:

  \begin{description}

     \item \textbf{Linear Undo and Redo} Previous states of the drawing are
     stored in a \emph{history list} which may become arbitrarily long. The last
     command in the list can be undone. When it is undone, it is removed from
     the history list and put into a \emph{redo list}. The undo operation can be
     repeated until the history list is empty (and all the commands are in the
     redo list). The last command put into the redo list can be redone and again
     be appended to the history list without affecting the redo list.

     \item \textbf{Selective Undo and Redo} The ability to affect only the
     objects in the current selection when using undo and redo.

  \end{description} *)
