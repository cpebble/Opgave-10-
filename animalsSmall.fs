
module animals
open System 
open System


type symbol = char
type position = int * int
type neighbour = position * symbol

let mSymbol : symbol = 'm'
let wSymbol : symbol = 'w'
let eSymbol : symbol = ' '
let rnd = System.Random ()

/// An animal is a base class. It has a position and a reproduction counter.
type animal (symb : symbol, repLen : int) =
  let mutable _reproduction = rnd.Next(1,repLen)
  let mutable _pos : position option = None
  let _symbol : symbol = symb

  member this.symbol = _symbol
  member this.position
    with get () = _pos
    and set aPos = _pos <- aPos
  member this.reproduction = _reproduction
  member this.updateReproduction () =
    _reproduction <- _reproduction - 1
  member this.resetReproduction () =
    _reproduction <- repLen
  override this.ToString () =
    string this.symbol

/// A moose is an animal
type moose (repLen : int) =
  inherit animal (mSymbol, repLen)

  member this.tick () : moose option =
    None // Intentionally left blank. Insert code that updates the moose's age and optionally an offspring.

/// A wolf is an animal with a hunger counter
type wolf (repLen : int, hungLen : int) =
  inherit animal (wSymbol, repLen)
  let mutable _hunger = hungLen

  member this.hunger = _hunger
  member this.updateHunger () =
    _hunger <- _hunger - 1
    //if _hunger <= 0 then
      //this.position <- None // Starve to death
  member this.resetHunger () =
    _hunger <- hungLen
  member this.tick () : wolf option =
    None // Intentionally left blank. Insert code that updates the wolf's age and optionally an offspring.

/// A board is a chess-like board implicitly representedy by its width and coordinates of the animals.
type board =
  {width : int;
   mutable moose : moose list;
   mutable wolves : wolf list;}

/// An environment is a chess-like board with all animals and implenting all rules.
type environment (boardWidth : int, NMooses : int, mooseRepLen : int, NWolves : int, wolvesRepLen : int, wolvesHungLen : int) =
  let _board : board = { //et board består af to lister, der hver især indeholder mooses og wolves 
    width = boardWidth;
    moose = List.init NMooses (fun i -> moose(mooseRepLen)); // en liste med NMooses-antal mooses, der alle har replen som mooseReplen
    wolves = List.init NWolves (fun i -> wolf(wolvesRepLen, wolvesHungLen)); //en liste med NWolf-antal wolves, der alle har wolvesreplen og wolfesHunglen
  }

  /// Project the list representation of the board into a 2d array.
  let draw (b : board) : char [,] = //tager et board som input 
    let arr = Array2D.create<char> boardWidth boardWidth eSymbol //laver et board som et 2D-array, hvor alle felter starter med at indeholde eSymbol 
    for m in b.moose do // looper igennem boardets moose-liste 
      Option.iter (fun p -> arr.[fst p, snd p] <- mSymbol) m.position //indsætter dyret på dens position i boardet 
    for w in b.wolves do
      Option.iter (fun p -> arr.[fst p, snd p] <- wSymbol) w.position
    arr
  let shuffleR (r : Random) xs = xs |> List.sortBy (fun _ -> r.Next())


  /// return the coordinates of any empty field on the board.
  let anyEmptyField (b : board) : position = //finder det første tomme felt på boardet 
    let arr = draw b
    let mutable i = rnd.Next b.width
    let mutable j = rnd.Next b.width
    while arr.[i,j] <> eSymbol do
      i <- rnd.Next b.width
      j <- rnd.Next b.width
    (i,j)

  let _findAllNeighboor (position: position) = //returnerer alle nabopladser for et dyr 
    let i,j = position 
    let allNeighboors : position list = [(i - 1, j - 1); (i - 1, j); (i - 1, j + 1); (i, j - 1); (i, j + 1); (i + 1, j - 1); (i + 1, j); (i + 1, j + 1)]
    let allValiedNeighboors : position list = List.filter (fun x -> (fst x >= 0 && fst x <= (boardWidth - 1) && snd x >= 0 && snd x <= (boardWidth - 1))) allNeighboors 
    shuffleR rnd allValiedNeighboors 
  
  let getAllPositons (b: board) = //en liste med alle optaget positioner på boardet 
    let mutable listWithPosition = []
    for i in 0..(_board.moose.Length - 1) do   
      listWithPosition <- (_board.moose.[i].position.Value) :: listWithPosition
    for j in 0..(_board.wolves.Length - 1) do   
      listWithPosition <- (_board.wolves.[j].position.Value) :: listWithPosition 
    listWithPosition 

  let getAllAnimals (b: board) = //en liste med alle optaget pladser og hvilke slags dyr der står på pladserne 
    let mutable listWithAnimals = []
    for i in 0..(_board.moose.Length - 1) do   
      listWithAnimals <- (_board.moose.[i].position, 'm') :: listWithAnimals
    for j in 0..(_board.wolves.Length - 1) do   
      listWithAnimals <- (_board.wolves.[j].position, 'w') :: listWithAnimals 
    listWithAnimals 
    

  // populate the board with animals placed at random.
  do for m in _board.moose do //giver hvert dyr en position på boardet ved at loope igennem de to lister 
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)


  let mutable allAnimals = getAllAnimals _board //indeholder en list med alle positioner og de dyr, der står på positionerne
  //denne skal updateres undervejs 


  let wolfTick (wolf: wolf) = 

    let thisWolf = wolf 
    if thisWolf.hunger <= 0 then    
      allAnimals <- List.filter (fun (x: position option * char) -> fst x <> thisWolf.position) allAnimals //filtre den døde ulv væk fra allAnimals
      _board.wolves <- List.filter (fun (x: wolf) -> x <> thisWolf) _board.wolves  


    else     
      let allNeighboors = _findAllNeighboor thisWolf.position.Value 
      let moosesBeside = (List.filter (fun (x: moose) -> List.contains x.position.Value allNeighboors) _board.moose) |> shuffleR rnd  //en liste med alle mooses placeret på nabopladserne 
      let oldPositon = thisWolf.position 
      if not moosesBeside.IsEmpty then   
        let newPosition = moosesBeside.[0].position 
        thisWolf.position <- newPosition 
        allAnimals <- List.filter (fun (x: position option * char) -> fst x <> oldPositon) allAnimals
        allAnimals <- List.filter (fun (x: position option * char) -> fst x <> newPosition) allAnimals //filtre moosen der er blevet spist fra allAnimals
        _board.moose <- List.filter (fun (x: moose) -> x.position <> newPosition) _board.moose //filtre moosen der lige er blevet spist væk 
        allAnimals <- allAnimals @ [newPosition, 'w']
        thisWolf.resetHunger()
        thisWolf.updateReproduction()

      else if thisWolf.reproduction <= 0 then 
        let position = anyEmptyField _board 
        let newWolf = new wolf(wolvesRepLen, wolvesHungLen)
        newWolf.position <- Some(position) 
        thisWolf.resetReproduction () 
        allAnimals <- allAnimals @ [Some(position), 'w'] //tilføjer den nye ulv til listen med ulve 
        _board.wolves <- _board.wolves @ [newWolf] 
        thisWolf.updateHunger()

      else 
        let allPositions = getAllPositons _board 
        let mutable isFree = false
        let mutable i = 0 
        let mutable newPosition = None 
        while not isFree && i <= (allNeighboors.Length - 1) do   
          let mutable currentNeighbor = allNeighboors.[i] 
          if not (List.contains currentNeighbor allPositions) then   
            isFree <- true 
            newPosition <- Some(currentNeighbor) 
          else 
            i <- i + 1 
        if newPosition.IsSome then  
          allAnimals <- List.filter (fun (x: position option * char) -> fst x <> oldPositon) allAnimals
          thisWolf.position <- newPosition 
          allAnimals <- allAnimals @ [newPosition, 'w']
        thisWolf.updateHunger ()
        thisWolf.updateReproduction()    



  let mooseTick (moose: moose) = 
    let thisMoose = moose 
    if thisMoose.reproduction <= 0 then //tjekker om den er klar til at føde
        let position = anyEmptyField _board //finder et tomt felt på brættet 
        let newMoose = new moose(mooseRepLen) //laver en ny moose-instance 
        newMoose.position <- Some(position)
        allAnimals <- allAnimals @ [(Some(position), 'm')] //tilføjer den nye moose position til alle allAnimals
        thisMoose.resetReproduction () 
        _board.moose <- _board.moose @ [newMoose] //sammensætter den nye moose med listen med de eksisterende mooses
      
    else 
      let allPositions = getAllPositons _board //indeholder alle optaget pladser på boardet 
      let allNeighboors = _findAllNeighboor thisMoose.position.Value //indeholder en liste med alle nabopladser
      let mutable isFree = false 
      let mutable i = 0 
      let mutable newPosition = None 
      while not isFree && i <= (allNeighboors.Length - 1) do //looper indtil den når igennem hele listen med naboer eller finder en ledig plads
        let mutable currentNeighbor = allNeighboors.[i] //indeholder den nuværende plads, vi tjekker om er ledig
        if not (List.contains currentNeighbor allPositions) then //tjekker om listen med alle optaget pladser indeholder currentNeighbor
          isFree <- true                                         // hvis ikke den gør det ændres isFree til true og mooses position ændres til den nye position 
          newPosition <- Some(currentNeighbor)
        else 
          i <- i + 1
      if newPosition.IsSome then 
        let oldPositon = thisMoose.position 
        allAnimals <- List.filter (fun (x: position option * char) -> fst x <> oldPositon) allAnimals //filtre den gamle position væk 
        thisMoose.position <- newPosition 
        allAnimals <- allAnimals @ [newPosition, 'm'] 
      thisMoose.updateReproduction()

     

  member this.size = boardWidth*boardWidth
  member this.count = _board.moose.Length + _board.wolves.Length
  member this.board = _board
  member this.mooseLength = _board.moose.Length 
  member this.wolvesLength = _board.wolves.Length 


  member this.tick() = 
    let mutable allAnimalsInTick = allAnimals // de dyr vi ønsker at foretage træk på nu 

    let randomOrder = shuffleR rnd allAnimalsInTick 

    
    
    for i in 0..(randomOrder.Length - 1) do 
      let thisAnimal = randomOrder.[i] 
      if (List.contains thisAnimal allAnimals) then  
        if (snd thisAnimal) = 'm' then 
          let thisMoose = List.find (fun (x: moose) -> x.position = (fst thisAnimal)) _board.moose  
          mooseTick thisMoose 
        if (snd thisAnimal) = 'w' then    
          let thisWolf = List.find (fun (x: wolf) -> x.position = (fst thisAnimal)) _board.wolves 
          wolfTick thisWolf 

      

  override this.ToString () =
    let arr = draw _board
    let mutable ret = "  "
    for j = 0 to _board.width-1 do
      ret <- ret + string (j % 10) + " "
    ret <- ret + "\n"
    for i = 0 to _board.width-1 do
      ret <- ret + string (i % 10) + " "
      for j = 0 to _board.width-1 do
        ret <- ret + string arr.[i,j] + " "
      ret <- ret + "\n"
    ret

  //******** test *********

<<<<<<< HEAD
=======
  //Test af findAllNeigboors
  member this.testFindAllNeigboors (pos: position) (exceptedOutput: position list) = 
    let actuelOutput = _findAllNeighboor pos 
    let mutable hasAll = true 
    for i in 0..(exceptedOutput.Length - 1) do 
      if not (List.contains exceptedOutput.[i] actuelOutput) then   
        hasAll <- false 
    hasAll 

  //Test af testGetAllPositions 
  member this.testGetAllPositions (b: board) (exceptedOutput: position list) = 
    let actuelOutput = getAllPositons b 
    exceptedOutput = actuelOutput 


  member this.testgetAllAnimals (b: board) (exceptedOutput: (position option * char) list) = 
    let actuelOutput = getAllAnimals b 
    exceptedOutput = actuelOutput 

  member this.testWolfTickHunger (w: wolf) = //exceptedLength er længden på den forventede liste
    let exceptedLength = _board.wolves.Length - 1 
    wolfTick w 
    _board.wolves.Length = exceptedLength && not(List.contains w _board.wolves) //listen skal gerne være en mindre, da ulven er død 
  
  member this.testWolfTickReproduction (w: wolf)  = 
    let exceptedLength = _board.wolves.Length + 1 
    printfn"Forventet antal ulve: %A" exceptedLength
    wolfTick w 
    printfn"faktiske antal ulve: %A" _board.wolves.Length 
    _board.wolves.Length = exceptedLength && w.reproduction = wolvesRepLen //Listen skal gerne være en længere, da der er tilføjet en ny ulv

  
  member this.testWolfTickEat (w: wolf) = 
    let numberOfWolvesBefore = _board.wolves.Length 
    let numberOfMoosesBefore = _board.moose.Length 
    wolfTick w 
    _board.wolves.Length = numberOfWolvesBefore && _board.moose.Length = numberOfMoosesBefore - 1 && w.hunger = wolvesHungLen

  member this.testWolfTickMove (w: wolf) = 
    let numberOfWolvesBefore = _board.wolves.Length 
    let oldPositon = w.position.Value
    let allNeighboors = _findAllNeighboor w.position.Value 
    wolfTick w 
    let newPosition = w.position.Value
    _board.wolves.Length = numberOfWolvesBefore && not(List.contains oldPositon (getAllPositons _board)) && newPosition <> oldPositon && (List.contains newPosition allNeighboors)
     
  member this.testMooseTickReproduction (m: moose)  = 
    let exceptedLength = _board.moose.Length + 1 
    printfn"Forventet antal elge: %A" exceptedLength
    mooseTick m 
    printfn"Faktiske antal elge: %A" _board.moose.Length
    _board.moose.Length = exceptedLength && m.reproduction = mooseRepLen 
  
  member this.testMooseTickMove (m: moose) = 
    let numberOfMoosesBefore = _board.moose.Length 
    let oldPositon = m.position.Value 
    let allNeighboors = _findAllNeighboor m.position.Value 
    mooseTick m 
    let newPosition = m.position.Value 
    _board.moose.Length = numberOfMoosesBefore && not(List.contains oldPositon (getAllPositons _board)) && newPosition <> oldPositon && (List.contains newPosition allNeighboors) 



//***** Test ****
  
let newBoard = new environment(5, 5, 5,  2, 5, 2) 
let newBoard2 = new environment(5, 5, 5,  2, 5, 2) 
let newBoard3 = new environment(5, 5, 5,  2, 5, 2) 
let newBoard4 = new environment(5, 5, 5,  2, 5, 2) 
let newBoard5 = new environment(5, 5, 5,  2, 5, 2) 
let newBoard6 = new environment(5, 5, 5,  2, 5, 2) 
let newBoard7 = new environment(5, 5, 5,  2, 5, 2) 



//Test af testFindAllNeigboors
let posList1 : position list = [(1,1); (1,2); (1,3); (2,1); (2,3); (3,1); (3,2); (3,3)]
let posList2 : position List = [(1,0);(0,1);(1,1)]

printfn"Test af FindAllNeighboors" 
printfn"%A" (newBoard.testFindAllNeigboors (2,2) posList1)
printfn"%A" (newBoard.testFindAllNeigboors (0,0) posList2)


//Test af testWolfTickHunger
let testWolfTickHunger (b: environment) = 
  let wolfInstans = b.board.wolves.[0] 
  while  wolfInstans.hunger <> 0 do 
    wolfInstans.updateHunger()

  printfn"testWolfTickHunger" 
  printfn"%A"(b.testWolfTickHunger wolfInstans)  


//Test af testWolfTickEat
let testWolfTickEat (b: environment) = 
  let wolfInstans = b.board.wolves.[0] 
  let mooseInstans = b.board.moose.[0]
  wolfInstans.resetHunger()
  wolfInstans.position <- Some(1,1)
  mooseInstans.position <- Some(1,2) 

  printfn"testWolfTickEat" 
  printfn"%A" (b.testWolfTickEat wolfInstans)


//Test af testWolfTickReproduction  

let testWolfTickReproduction (b: environment) = 
  b.board.moose <- []
  let wolfInstans = b.board.wolves.[0] 
  while wolfInstans.reproduction > 0 do 
    wolfInstans.updateReproduction()

  printfn"testWolfTickReproduction" 
  printfn"%A" (b.testWolfTickReproduction wolfInstans)


//Test af testMooseTickReproduction 
let testMooseTickReproduction (b: environment) = 
  let mooseInstans = b.board.moose.[0]
  while mooseInstans.reproduction <> 0 do 
    mooseInstans.updateReproduction()
  
  printfn"testMooseTickReproduction"
  printfn"%A" (b.testMooseTickReproduction mooseInstans) 


//Test af testWolfTickMove 

let testWolfTickMove (b: environment) = 
  let wolfInstans = b.board.wolves.[0]
  wolfInstans.resetHunger()
  wolfInstans.resetReproduction()
  b.board.wolves <- [wolfInstans]
  b.board.moose <- []

  printfn"testWolfTickMove"
  printfn"%A"(b.testWolfTickMove wolfInstans)

//Test af testMooseTickMove

let testMooseTickMove (b: environment) = 
  let mooseInstans = b.board.moose.[0]
  mooseInstans.resetReproduction()
  b.board.moose <- [mooseInstans]
  b.board.wolves <- []

  printfn"testMooseTickMove"
  printfn"%A" (b.testMooseTickMove mooseInstans)


testWolfTickHunger newBoard2
testWolfTickEat newBoard3
testWolfTickReproduction newBoard4
testMooseTickReproduction newBoard5
testWolfTickMove newBoard6
testMooseTickMove newBoard7



//******** app *********


>>>>>>> 76cb662a554e380b7d16ae55bad8f57eee5cf6a3
let writeToFile (lst:'a List) (filename:string) :unit =
  use file = (System.IO.File.CreateText filename)
  for (a,b,c) in lst do
    file.Write (sprintf "%A:%A:%A \n" a b c)

//Kalder ticks et bestemt antal gange og laver en liste bestående af antal wolves og mooses per tick 
let app (boardWidth : int) (nMooses : int) (mooseRepLen : int) (nWolves : int) (wolvesRepLen : int) (wolvesHungLen : int) (numberofTicks: int) = 
  let newBoard = new environment (boardWidth, nMooses, mooseRepLen, nWolves, wolvesRepLen, wolvesHungLen)
  let mutable listWithTicks = []
  for i in 0..(numberofTicks - 1) do 
    newBoard.tick()
    listWithTicks <- listWithTicks @ [(i, newBoard.mooseLength, newBoard.wolvesLength)] 
  printfn "%A" listWithTicks
  writeToFile listWithTicks "output.txt"

//app 5 5 5 2 5 2 8
[<EntryPoint>]
let main argv =
  let mutable arg_list = []
  try
    for arg in argv do
      let refVar = ref 0 
      if (Int32.TryParse(arg,refVar)) then
        arg_list <- arg_list @[!refVar]
      else
        failwith "Conversion failed"
  with
    |ex -> printfn "%A" ex

  try
    app arg_list.[0] arg_list.[1] arg_list.[2] arg_list.[3] arg_list.[4] arg_list.[5] arg_list.[6]
  with
  |ex -> printfn "somethings fucked up"
  printfn "%A" (arg_list)
  0 
    


