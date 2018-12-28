module animals

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
    if _hunger <= 0 then
      this.position <- None // Starve to death
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
type environment (boardWidth : int, NMooses : int, mooseRepLen : int, NWolves : int, wolvesRepLen : int, wolvesHungLen : int, verbose : bool) =
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
    let allNeighboors : position list = [(i - 1, j - 1); (i - 1, j); (i - 1, j + 1); (i, j - 1); (i, j + 1); (i + 1, j + 1); (i - 1, j + 1); (i + 1, j - 1)]
    let allValiedNeighboors : position list = List.filter (fun x -> (fst x >= 0 && fst x < boardWidth && snd x >= 0 && snd x < boardWidth)) allNeighboors 
    allValiedNeighboors 
  
  let getAllPositons (b: board) = //en liste med alle optaget positioner på boardet 
    let mutable listWithPosition = []
    for i in 0..(_board.moose.Length - 1) do   
      listWithPosition <- (_board.moose.[i].position.Value) :: listWithPosition
    for j in 0..(_board.wolves.Length - 1) do   
      listWithPosition <- (_board.wolves.[j].position.Value) :: listWithPosition 
    listWithPosition 
    

  // populate the board with animals placed at random.
  do for m in _board.moose do //giver hvert dyr en position på boardet ved at loope igennem de to lister 
       m.position <- Some (anyEmptyField _board)
  do for w in _board.wolves do
       w.position <- Some (anyEmptyField _board)

  let mooseTick (moose: moose) (boardLength: int ) = 
    let numberOfMooses = boardLength 
    let thisMoose = moose 
    if thisMoose.reproduction <= 0 then //tjekker om den er klar til at føde
        let position = anyEmptyField _board //finder et tomt felt på brættet 
        let newMoose = new moose(mooseRepLen) //laver en ny moose-instance 
        newMoose.position <- Some(position)
        thisMoose.resetReproduction () //--
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
          thisMoose.position <- newPosition 
          thisMoose.updateReproduction()


  member this.size = boardWidth*boardWidth
  member this.count = _board.moose.Length + _board.wolves.Length
  member this.board = _board


  member this.tick() = 
    let numberOfMooses = _board.moose.Length 
    let numberOfWolfes = _board.wolves.Length 
  
    //suffle allAnimals
    for m in 0..(numberOfMooses - 1) do   //kalder moose.tick  
      let mutable thismoose = _board.moose.[m]
      mooseTick thismoose numberOfMooses 

    //for i in 0..(numberOfWolfes - 1) do   //kalder wolf.tick  
     // printfn"%A" _board.wolves.[i]


  member this.tick2 () = 
    printfn"Med alle ulve:%A" _board.wolves  
    _board.wolves <- List.filter (fun (x: wolf) -> x.position.IsSome) _board.wolves 
    printfn"Kun med levende ulve:%A" _board.wolves 

    let numberOfWolfes = _board.wolves.Length 
    for w in 0..(numberOfWolfes - 1) do   

      let mutable thisWolf = _board.wolves.[w]

      if thisWolf.reproduction <= 0 then   
        let position = anyEmptyField _board 
        let newWolf = new wolf(wolvesRepLen, wolvesHungLen)
        newWolf.position <- Some(position)
        thisWolf.updateHunger ()
        thisWolf.resetReproduction ()
        _board.wolves <- _board.wolves @ [newWolf] //den nye ulv bliver sat tilsidst i listen af ulve 

      (*else 
          let allNeighboors = _findAllNeighboor thisWolf.position.Value //finder en liste med dennes naboer 
          let moosesBeside = List.filter (fun (x: moose) -> List.contains x.position.Value allNeighboors) _board.moose //en liste med alle mooses placeret på nabopladserne 
          printfn"mooses ved siden af %A" moosesBeside //  TEST   
          if (not moosesBeside.IsEmpty) then   
            printfn"Gamle position%A" thisWolf.position
            let newPosition = moosesBeside.[0].position //vælger den første moose i listen af mooses ved siden af
            thisWolf.position <- newPosition
            printfn"Nye position%A" thisWolf.position
            _board.moose <- List.filter (fun (x: moose) -> x.position <> newPosition) _board.moose //filtre moosen der lige er blevet spist væk 
            thisWolf.resetHunger ()*) 
        else 
          let allPositions = getAllPositons _board 
          printfn"alle optaget positioner: %A" allPositions 
          let allNeighboors = _findAllNeighboor thisWolf.position.Value //finder en liste med dennes naboer 
          let mutable newPosition = None 
          let mutable isFree = false 
          let mutable i = 0 
          while not isFree && i <= (allNeighboors.Length - 1) do  
            let mutable currentNeighbor = allNeighboors.[i] 
            if not (List.contains currentNeighbor allPositions) then  
                isFree <- true 
                newPosition <- Some(currentNeighbor) 
            else   
              i <- i + 1 
            if newPosition.IsSome then  
              thisWolf.position <- newPosition 
            else 
              thisWolf.position <- thisWolf.position 
          printfn"thisWolfs position: %A" thisWolf.position
          thisWolf.updateReproduction () 
          thisWolf.updateHunger () 




          (*    else 
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
          thisMoose.position <- newPosition 
          thisMoose.updateReproduction()*) 




    (*member this.tick2 () = 
      _board.wolves <- List.filter (fun (x: wolf) -> x.position.IsSome) _board.wolves //sletter alle wolfes, vis position er None
      let numberOfWolfes = _board.wolves.Length 
      for w in 0..(numberOfWolfes - 1) do 
        let mutable thisWolf = _board.wolves.[w] 

        if thisWolf.reproduction <= 0 then 
          let position = anyEmptyField _board 
          let newWolf = new wolf(wolvesRepLen, wolvesHungLen)  
          newWolf.position <- Some(position)
          thisWolf.resetReproduction () 
          thisWolf.updateHunger()
          _board.wolves <- _board.wolves @ [newWolf]
          
        else 
          let allNeighboors = _findAllNeighboor thisWolf.position.Value //indeholder en liste med alle dens naboer 
          let moosesBeside = List.filter (fun (x: moose) -> (List.contains x.position.Value allNeighboors)) _board.moose //en liste med alle mooses, der er placeret på en af nabopladserne 
          if (not moosesBeside.IsEmpty) then   
            let newPosition = moosesBeside.[0].position //vælger den første mooses 
            thisWolf.position <- newPosition // sætter thisWolfs position til denne 
            _board.moose <- List.filter (fun (x: moose) -> x.position <> newPosition) _board.moose //updaterer moose listen, hvor den moose med denne position filtres væk 
            thisWolf.updateHunger ()
            thisWolf.updateReproduction () 

          else   
            let allPositions = getAllPositons _board 
            let mutable isFree = false 
            let mutable i = 0 
            let mutable newPosition = None 
            while not isFree && i <= (allNeighboors.Length - 1) do //looper indtil den når igennem hele listen med naboer eller finder en ledig plads
              let mutable currentNeighbor = allNeighboors.[i] //indeholder den nuværende plads, vi tjekker om er ledig
              if not (List.contains currentNeighbor allNeighboors) then //tjekker om listen med alle optaget pladser indeholder currentNeighbor
                isFree <- true                                         // hvis ikke den gør det ændres isFree til true og mooses position ændres til den nye position 
                newPosition <- Some(currentNeighbor)
              else 
                i <- i + 1
              if newPosition.IsSome then 
                thisWolf.position <- newPosition 
                thisWolf.updateReproduction()  
                thisWolf.updateHunger ()    *)          

            


            
   

      

     // Intentionally left blank. Insert code that process animals here.
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



let newBoard = new environment(4, 2, 10,  2, 10, 5, true) 
printfn"%A" (newBoard.ToString ()) 



for i in 0..11 do 
  printfn"Før update:"
  printfn"%A" (newBoard.ToString ()) 
  printfn"Update %A" i 
  newBoard.tick2()
  printfn"%A" (newBoard.ToString ())
