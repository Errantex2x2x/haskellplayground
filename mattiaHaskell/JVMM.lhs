\documentclass[11pt, a4paper]{article}

\input{../setup}

\title{Implementazione della Java Virtual Mini-Machine}

\begin{document}

\maketitle
\thispagestyle{fancy}

\newcommand{\JVM}{\texttt{JVM}\xspace}
\newcommand{\JVMM}{\texttt{JVMM}\xspace}

\newcommand{\PUSH}[1]{\mathtt{PUSH}~#1}
\newcommand{\LOAD}[1]{\mathtt{LOAD}~#1}
\newcommand{\STORE}[1]{\mathtt{STORE}~#1}
\newcommand{\OP}[1]{\mathtt{OP}~{#1}}
\newcommand{\IF}[2]{\mathtt{IF}~{#1}~{#2}}
\newcommand{\RETURN}{\mathtt{RETURN}}

\newcommand{\F}{\mathcal{F}}
\newcommand{\R}{\mathcal{R}}

\section{Descrizione del problema}

In questo caso di studio realizziamo un esecutore per la Java Virtual
Mini-Machine (\JVMM), una versione minimale ma comunque espressiva
della Java Virtual Machine.
%
In particolare, consideriamo il seguente insieme di istruzioni
\begin{center}
  \begin{tabular}{|l|r@{~}c@{~}l|p{0.65\textwidth}|}
    \hline
    $\PUSH{v}$ & & $\to$ & $v$ & Inserisce il valore $v$ sullo stack
    \\
    \hline
    $\LOAD{x}$ & & $\to$ & $v$ & Legge il valore $v$ dallo slot di $x$ e lo inserisce sullo stack
    \\
    \hline
    $\STORE{x}$ & $v$ & $\to$ & & Rimuove $v$ dallo stack e lo scrive nello slot di $x$
    \\
    \hline
    $\OP\F$ & $w$,$v$ & $\to$ & $\F(v,w)$ & Rimuove $w$ e poi $v$ dallo stack e inserisce $\F(v,w)$ sullo stack
    \\
    \hline
    $\IF\R\ell$ & $w$,$v$ & $\to$ & & Rimuove $w$ e poi $v$ dallo stack e salta a $\ell$ se $\R{(v, w)}$
    \\
    \hline
    $\RETURN$ & $v$ & $\to$ & & Rimuove $v$ dallo stack e termina l'esecuzione con risultato $v$
    \\
    \hline
  \end{tabular}
\end{center}
%
in cui $v$ e $w$ rappresentano \emph{valori} della \JVMM che limitiamo
ai numeri interi, $x$ rappresenta il nome di una variabile locale,
$\F$ rappresenta una funzione binaria su valori (somma, sottrazione,
moltiplicazione, ecc.)  ed $\R$ rappresenta una relazione binaria tra
valori (uguale a, minore di, ecc.).
%
Per ogni istruzione, la tabella mostra gli eventuali argomenti, i
valori che l'istruzione si aspetta di trovare in cima allo stack prima
di essere eseguita (a sinistra del simbolo $\to$) ed i valori che
l'istruzione inserisce sullo stack durante la sua esecuzione (a destra
del simbolo $\to$). Quando pi\`u valori sono indicati, si intende che
quello pi\`u a sinistra \`e quello in cima allo stack.

Ad esempio, il programma
\[
  \begin{array}{rl}
    & \LOAD{x} \\
    & \LOAD{y} \\
    & \IF{<}{\ell} \\
    & \LOAD{x} \\
    & \RETURN \\
    \ell: & \LOAD{y} \\
    & \RETURN
  \end{array}
\]
calcola il massimo tra i valori memorizzati nelle variabili $x$ ed $y$.

\section{Definizione delle strutture fondamentali}

Nella \JVMM vi sono tre strutture fondamentali:
\begin{itemize}
\item lo \emph{stack}, usato come contenitore di dimensione variabile
  per valori temporanei, dal quale le istruzioni rimuovono valori e
  sul quale le istruzioni inseriscono risultati;
\item il \emph{frame}, definito come una collezione finita di
  \emph{slot}, uno per ogni variabile usata dal programma. Uno slot
  \`e una cella di memoria contenente il valore della variabile
  corrispondente;
\item il \emph{codice}, ovvero la sequenza di istruzioni che
  compongono il programma da eseguire.
\end{itemize}

Implementare la \JVMM significa stabilire e definire opportune
rappresentazioni per queste strutture. Iniziamo definendo due
\emph{alias} di tipo per valori e stack:

\begin{code}
module JVMM where
type Value  = Int
type Stack  = [Value]
\end{code}

In questa versione della \JVMM gli unici valori sono numeri interi,
dunque il tipo \HA{Value} non \`e altro che un nome alternativo (e
pi\`u significativo) del tipo \HA{Int}. Inoltre, possiamo usare le
liste come comoda rappresentazine per lo stack della \JVMM. In tal
modo, useremo l'operatore \HA{:} per inserire un valore in cima allo
stack ed il pattern matching per rimuovere uno o pi\`u valori dallo
stack.

Scegliamo di rappresentare un frame come una lista di coppie $(x,v)$
in cui $x$ \`e il nome di una variabile e $v$ il valore assegnato a
quella variabile nello slot corrispondente. Ad esempio, la lista
\begin{Haskell}
  [("m", 2), ("n", 3)]
\end{Haskell}
rappresenta un frame con due variabili \HA{m} (con valore assegnato
\HA{2}) ed \HA{n} (con valore assegnato \HA{3}). In generale questa
lista contiene coppie in cui la prima componente svolge il ruolo di
``chiave'', mentre la seconda componente rappresenta il valore
associato a quella chiave. Per questo motivo, le liste con tale
struttura sono chiamate \textbf{liste associative}.

Definiamo dunque due alias di tipo per i nomi di variabili e i frame:
\begin{code}
type Name   = String
type Frame  = [(Name, Value)]
\end{code}

Per motivi di efficienza, nella \JVM gli slot di un frame vengono
acceduti per mezzo di un numero invece che di una stringa ed \`e
compito del compilatore Java tradurre i nomi delle variabili in
numeri. La scelta di usare le stringhe nell'interprete della \JVMM ha
un costo in termini di efficienza, ma rende i programmi pi\`u
leggibili e l'esecutore pi\`u semplice da realizzare.

A differenza dello stack, in cui le operazioni di inserimento e
rimozione di elementi corrispondono a semplici operatori e costrutti
di Haskell, nel caso dei frame e, pi\`u in generale, delle liste
associative \`e necessario definire due funzioni ausiliarie che
chiameremo \HA{find} e \HA{update}. La prima ritorna il valore
associato ad una chiave data in una lista associativa. La seconda
``modifica'' il valore associato ad una chiave data in una lista
associativa. Come al solito, la modifica non \`e da intendersi in modo
imperativo. Al contrario, \HA{update} crea una nuova lista associativa
aggiornata.

Le funzioni \HA{find} e \HA{update} sono definite di seguito:
\begin{code}
find :: Eq a => a -> [(a, b)] -> b
find x ((y, v) : _) | x == y = v
find x (_ : ys) = find x ys
\end{code}

\begin{code}
update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update x v [] = [(x, v)]
update x v ((y, _) : ys) | x == y = (x, v) : ys
update x v ((y, w) : ys) = (y, w) : update x v ys
\end{code}

Notiamo un'asimmetria importante tra \HA{find} e \HA{update}: la prima
funzione non \`e definita nel caso in cui non ci sia alcuna
associazione per la chiave cercata. Anche per questo motivo \`e stato
scelto il nome \HA{find} invece di, ad esempio, \HA{search}. Al
contrario, la funzione \HA{update} \emph{aggiunge} una associazione
per la chiave \HA{x} se questa non viene trovata nella lista
associativa. Questo comportamento di \HA{update} ci consente di
iniziare l'esecuzione del codice usando il frame vuoto.

La rappresentazione pi\`u naturale del bytecode \`e come lista di
istruzioni:
\begin{code}
type Code = [Instruction]
\end{code}

Per quanto riguarda le istruzioni, ci troviamo di fronte a entit\`a
che possono essere di tipologia differente (\HA{PUSH}, \HA{LOAD},
\HA{STORE}, ecc.) e tali per cui ogni tipologia pu\`o avere zero o
pi\`u argomenti. Ad esempio, l'istruzione \HA{PUSH} ha come argomento
un valore, sia \HA{LOAD} che \HA{STORE} hanno come argomento il nome
di una variabile, e \HA{RETURN} non ha argomenti. Queste osservazioni
suggeriscono che per rappresentare una singola istruzione della \JVMM
\`e comodo definire un nuovo tipo di dato algebrico con tanti
costruttori quante sono le istruzioni da rappresentare. Ogni
costruttore sar\`a dotato di tanti argomenti quanti sono gli argomenti
dell'istruzione corrispondente. In codice Haskell abbiamo:
\begin{code}
data Instruction
  = PUSH Value
  | LOAD Name
  | STORE Name
  | OP (Value -> Value -> Value)
  | IF (Value -> Value -> Bool) Code
  | RETURN
\end{code}

Da notare che i costruttori \HA{OP} e \HA{IF} hanno un argomento di
tipo funzione che rappresenta l'operazione binaria (nel caso di
\HA{OP}) o la relazione (nel caso di \HA{IF}).  In particolare,
scegliamo di modellare una relazione binaria come una funzione a due
argomenti che ritorna un valore di tipo \HA{Bool}.

\section{Realizzazione dell'esecutore}

Modelliamo l'esecutore della \JVMM come una funzione \HA{run} che,
applicata a un frammento di codice e dunque a un valore di tipo
\HA{Code}, ritorna il risultato dell'esecuzione, ovvero il valore in
cima alla pila al momento dell'esecuzione dell'istruzione
\HA{RETURN}. Definiamo \HA{run} come specializzazione di una funzione
ausiliaria \HA{aux} che ha come argomenti, oltre al codice da
eseguire, anche lo stato corrente del frame e dello stack.

\begin{code}
run :: Code -> Value
run = aux [] []
  where
    aux :: Frame -> Stack -> Code -> Value
    aux _  (v : [])     (RETURN : [])         = v -- \label{RETURN}
    aux fr vs           (PUSH v : is)         = aux fr (v : vs) is -- \label{PUSH}
    aux fr vs           (LOAD x : is)         = aux fr (find x fr : vs) is -- \label{LOAD}
    aux fr (v : vs)     (STORE x : is)        = aux (update x v fr) vs is -- \label{STORE}
    aux fr (w : v : vs) (OP f : is)           = aux fr (f v w : vs) is -- \label{OP}
    aux fr (w : v : vs) (IF p is : _) | p v w = aux fr vs is -- \label{IF_TRUE}
    aux fr (_ : _ : vs) (IF _ _ : is)         = aux fr vs is -- \label{IF_FALSE}
\end{code}

La funzione ausiliaria \HA{aux} \`e definita per casi sulla prima
istruzione da eseguire. Da notare l'uso simultaneo del pattern
matching sui vari argomenti di \HA{aux} in modo da mettere in evidenza
gli eventuali operandi (sullo stack) usati dall'istruzione e la
continuazione del codice dopo l'istruzione stessa.
%
Nel caso di \HA{RETURN} (riga~\ref{RETURN}) l'esecuzione termina
restituendo il valore \HA{v} trovato in cima allo stack. Per come sono
specificati i pattern, \`e necessario che \HA{v} sia l'unico valore
sullo stack e che \HA{RETURN} sia l'ultima istruzione del codice.
%
L'istruzione \HA{PUSH} aggiorna lo stato dello stack inserendo in cima
l'argomento dell'istruzione stessa. Notare l'applicazione ricorsiva di
\HA{aux} in cui l'argomento che rappresenta lo stack viene
opportunamente modificato (riga~\ref{PUSH}).
%
Le istruzioni \HA{LOAD} e \HA{STORE} leggono e scrivono il frame,
rispettivamente (righe~\ref{LOAD}--\ref{STORE}).
%
L'istruzione \HA{OP} applica la funzione \HA{f} specificata come
argomento ai primi due operandi in cima allo stack, inserendo poi il
risultato sullo stack (riga~\ref{OP}). Occorre prestare attenzione al
fatto che l'operando in cima allo stack (\HA{w}) \`e di fatto il
secondo che \`e stato inserito, mentre l'operando immediatamente
sottostante (\HA{v}) \`e il primo. Dunque, \HA{f} viene applicata
prima a \HA{v} e poi a \HA{w}. Questo dettaglio \`e fondamentale per
quelle operazioni non commutative come la sottrazione o la divisione.
%
Ci sono due casi possibili per l'istruzione \HA{IF}, a seconda che la
condizione che determina il salto sia verificata oppure no
(righe~\ref{IF_TRUE}--\ref{IF_FALSE}). Anche qui occorre mettere in
evidenza i due operandi in cima allo stack e applicare il predicato
\HA{p} nell'ordine giusto.

A titolo di esempio riportiamo il codice di un metodo che calcola il
fattoriale del valore memorizzato nella variabile \HA{n}:

\begin{code}
fattoriale :: Code
fattoriale = init
  where
    init = PUSH 10 :
           STORE "n" :
           PUSH 1 : -- \label{res1}
           STORE "res" : -- \label{res2}
           loop -- \label{begin_loop}
    loop = LOAD "n" : -- \label{loop_start}
           PUSH 0 :
           IF (==) fine : -- \label{check_n}
           LOAD "n" : -- \label{update1}
           LOAD "res" :
           OP (*) :
           STORE "res" : -- \label{update2}
           LOAD "n" : -- \label{n1}
           PUSH 1 :
           OP (-) :
           STORE "n" : -- \label{n2}
           loop -- \label{loop_end}
    fine = LOAD "res" : -- \label{return1}
           RETURN : [] -- \label{return2}
\end{code}

Il codice segue la struttura classica dell'algoritmo iterativo per il
calcolo del fattoriale. Viene usata una variabile locale \HA{res}
inizializzata a 1 (righe~\ref{res1}--\ref{res2}) per memorizzare il
valore parziale del prodotto
$n \times (n - 1) \times \cdots \times 2 \times 1$. Il ciclo
dell'algoritmo (righe~\ref{loop_start}--\ref{loop_end}) inizia
controllando se \HA{n} \`e diventata 0
(righe~\ref{loop_start}--\ref{check_n}). In tal caso, \HA{res}
contiene il risultato
(righe~\ref{return1}--\ref{return2}). Altrimenti, \HA{res} viene
aggiornato moltiplicandolo per il valore corrente di \HA{n}
(righe~\ref{update1}--\ref{update2}), \HA{n} viene decrementato di 1
(righe~\ref{n1}--\ref{n2}) e si torna all'inizio del ciclo
(riga~\ref{loop_end}).

Una particolarit\`a interessante di questa definizione \`e che fa uso
di \textbf{liste infinite}. In particolare, si assegna il nome
\HA{loop} al codice corrispondente al ciclo dell'algoritmo
(riga~\ref{loop_start}) e, laddove occorre effettuare un ``salto'' a
quel codice, si scrive semplicemente tale nome (righe~\ref{begin_loop}
e~\ref{loop_end}).

Come esempio ulteriore riportiamo anche il codice di un metodo che
calcola il \HA{k}-esimo numero nella sequenza di Fibonacci:

\begin{code}
fibonacci :: Code
fibonacci = init
  where
    init = PUSH 0 :
           STORE "m" :
           PUSH 1 :
           STORE "n" :
           PUSH 10 :
           STORE "k" :
           loop
    loop = LOAD "k" :
           PUSH 0 :
           IF (==) fine :
           LOAD "n" :
           LOAD "n" :
           LOAD "m" :
           OP (+) :
           STORE "n" :
           STORE "m" :
           LOAD "k" :
           PUSH 1 :
           OP (-) :
           STORE "k" :
           loop
    fine = LOAD "m" :
           RETURN : []
\end{code}

\section{Esercizi}

\begin{exercise}
  Scrivere un frammento di bytecode per la \JVMM che calcola il
  massimo comun divisore di due numeri $m$ ed $n$ secondo l'algoritmo
  di Euclide. La soluzione dell'esercizio si trova in fondo al
  sorgente Literate Haskell di questo documento.
\end{exercise}

Le estensioni richieste nei seguenti esercizi sono tutte incorporate
nell'unico file \HA{JVMM.hs}.

\begin{exercise}
  Estendere la \JVMM con le istruzioni \JA{DUP}, \HA{SWAP}, \HA{POP} e
  \JA{NOP} la cui semantica \`e descritta alla pagina
  \url{https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings}.
  Scrivere alcuni semplici frammenti di bytecode per verificare la
  correttezza delle estensioni realizzate.
\end{exercise}

\begin{exercise}
  Per estendere la \JVMM in modo che possa operare anche su valori di
  tipo \HA{Float} \`e sufficiente modificare la definizione di
  \HA{Value} come segue
  \begin{Haskell}
    data Value = INT Int | FLOAT Float
  \end{Haskell}

  Apportare le modifiche necessarie ai frammenti di bytecode discussi
  in questo documento. Verificare il corretto funzionamento
  dell'estensione scrivendo un frammento di bytecode che calcola
  l'approssimazione del fattoriale di $n$ secondo la formula di
  Stirling.
\end{exercise}

\begin{exercise}
  Estendere la \JVMM con una istruzione \JA{UOP} per rappresentare
  operatori \emph{unari}. Ad esempio, tale istruzione deve rendere
  possibile modellare l'istruzione \JA{ineg} della \JVM.  Scrivere un
  semplice frammento di bytecode per verificare la correttezza
  dell'estensione.
\end{exercise}

\end{document}

\begin{code}
euclide :: Int -> Int -> Int
euclide x y = run init
  where
    n = "n"
    m = "m"
    init = PUSH x :
           STORE m :
           PUSH y :
           STORE n :
           loop
    loop = LOAD m :
           PUSH 0 :
           IF (==) fine :
           LOAD m :
           LOAD n :
           IF (<) less :
           LOAD m :
           LOAD n :
           OP (-) :
           STORE m :
           loop
    less = LOAD m :
           LOAD n :
           STORE m :
           STORE n :
           loop
    fine = LOAD n :
           RETURN : []
\end{code}
