{ Program: DockingProblem
  Purpose: A simple CLI application to calculate the heuristics and cost 
           from the initial state.
  Author: Harsh Jayprakash.
  Date: January 2023 (modified March 2025 to add documentation)
  License: MIT.
  Description:
    This program demonstrated my ability to calculate the cost and heuristics as part of 
    my Artificial Intelligence Module at the University of Winchester studying Computer 
    Science. The task involved a scenario where there were a number of loading bays, 
    containers and a crane to move a container between each bay. I chose the initial state 
    as:

            (!)


    [5]     [4]
    [1] [2] [3]
     A   B   C   D
    
    The goal could be anything as long as the containers 1, 2 and 3 were numerically 
    organised at a single loading bay. I chose the following goal state:
    
    [3]
    [2] [5]
    [1] [4]
     A   B   C   D

    I needed to work out the best possible route to reach the goal state from the initial
    state, with the least cost using a searching algorithm (such as A*). Honestly, I 
    struggled with this assignment. This program was a way for me to calculate the 
    heuristics and cost by adding a single line of code to indicate a movement.
}

program DockingProblem;

{$mode objfpc}
{$apptype console}
{$assertions on}

uses SysUtils;

const
    { Denotes the number of containers in the scenario. }
    CONTAINER_COUNT = 5;

    { Denotes the number of loading bays available in the scenario. }
    LOADING_BAY_COUNT = 4;

    { Denotes the maximum number of containers that could be stacked on one loading bay }
    CONTAINER_STACK_LIMIT = 3;

    { Denotes the maximum number of paths that can be taken. This was to set the upper 
      limit of the path array.
    }
    PATH_ALLOC_LIMIT = 50;

    { Denotes the time taken for the crane to move between loading bays. }
    LOADING_BAY_TIME = 3;

    { Denotes the maximum time taken for the crane to pick up a container from a loading 
      bay.
    }
    MAX_PICKUP_TIME = 4;

    { Denotes the time taken for the crane to drop a container onto a loading bay. }
    DROP_TIME = 2;

type
    { Denotes the what stack position of the loading bay the container 
      is at. Below shows a text-based diagram:

        [ ]   <-- Top    (Idx 2)
        [ ]   <-- Middle (Idx 1)
        [ ]   <-- Bottom (Idx 0)
      Loading Bay X
    }
    TStackPosition = (
        { A stack position that is unset or held by the crane. }
        Unassigned = -1, 
        { At the bottom position of the loading bay stack. }
        Bottom = 0, 
        { At the middle position of the loading bay stack. }
        Middle = 1, 
        { At the top position of the loading bay stack. }
        Top = 2
    );

    { Denotes a single container entity. }
    TContainer = record
        { The container id. }
        Identifer: Integer;
        { The loading bay the container is in. }
        InBay: Char;
        { The stack position in the specified loading bay. }
        BayPosition: TStackPosition;
        { If the container is picked up by the crane. }
        PickUpByCrane: Boolean;
    end;

    { Denotes a single loading bay. }
    TLoadingBay = record
        { The loading bay id. }
        Identifer: Char;
        { The stack of currently held containers (uses container id). }
        Stack: array [0..CONTAINER_STACK_LIMIT] of Integer;
        { The current number of containers. }
        ContainerCount: Integer;
    end;

    { Denotes the crane. }
    TCrane = record
        { The loading bay the crane is over. }
        AtBay: Char;
        { The id of the container the crane is holding. }
        Holding: Integer;
    end;

    { Denotes the move path. Variables prefixed with 'L' to avoid language 
      keyword conflicts.
    }
    TPath = record
        { The source loading bay. }
        LFrom: Char;
        { The destination loading bay. }
        LTo: Char;
    end;

    { Denotes the whole environment of the scenario. }
    TEnvironment = record
        { The loading bays. }
        LoadingBays: array [0..LOADING_BAY_COUNT] of TLoadingBay;
        { The containers. }
        Containers: array [0..CONTAINER_COUNT] of TContainer;
        { All paths taken. }
        Paths: array [0..PATH_ALLOC_LIMIT] of TPath;
        { Number of paths. }
        PathCount: Integer;
        { The crane. }
        Crane: TCrane;
        { The total cost of moves taken. }
        TotalCost: Integer;
    end;


{ Procedure: LoadingBayInit
  Purpose: Initialises the given loading bay.
  Parameters:
    [out] LoadingBay (TLoadingBay) - The loading bay to be initialised.
    [in]  Identifier (Char)        - The loading bay id.
}
procedure LoadingBayInit(var LoadingBay: TLoadingBay; Identifer: Char);
var Index: Integer;
begin
    LoadingBay.Identifer := Identifer;
    LoadingBay.ContainerCount := 0;
    for Index := 0 to (CONTAINER_STACK_LIMIT-1) do
        LoadingBay.Stack[Index] := 0;
end;

{ Procedure: CraneInit
  Purpose: Initialises the crane.
  Parameters:
    [out] Crane (TCrane) - The crane to be initialised.
}
procedure CraneInit(var Crane: TCrane);
begin
    Crane.AtBay := '0';
    Crane.Holding := 0;
end;

{ Procedure: ContainerInit
  Purpose: Initialises a container.
  Parameters:
    [out] Container  (TContainer) - The container to be initialised.
    [in]  Identifier (Integer)    - The container id.
}
procedure ContainerInit(var Container: TContainer; Identifer: Integer);
begin
    Container.Identifer := Identifer;
    Container.BayPosition := TStackPosition.Unassigned;
end;

{ Procedure: PathsInit
  Purpose: Initialise the paths array.
  Paramters:
    [out] Paths (Array of TPath) - The path array to be initialised.
}
procedure PathsInit(var Paths: array of TPath);
var Index: Integer;
begin
    for Index := 0 to PATH_ALLOC_LIMIT do
    begin 
        Paths[Index].LFrom := Char('0');
        Paths[Index].LTo := Char('0');
    end;
end;

{ Procedure: CraneAt
  Purpose: Sets the position of the crane.
  Parameters:
    [out] Crane      (TCrane)      - The crane.
    [in]  LoadingBay (TLoadingBay) - The loading bay the crane should move 
                                     to.
}
procedure CraneAt(var Crane: TCrane; LoadingBay: TLoadingBay);
begin
    Crane.AtBay := LoadingBay.Identifer;
end;

{ Procedure: ContainerAt
  Purpose: Sets the position of the container.
  Parameters:
    [out] Container  (TContainer)     - The container.
    [out] LoadingBay (TLoadingBay)    - The loading bay the container should be in.
    [in]  StackIndex (TStackPosition) - The position of the container on the loading bay 
                                        stack.
}
procedure ContainerAt(
    var Container: TContainer; var LoadingBay: TLoadingBay; 
    StackIndex: TStackPosition);
begin
    if StackIndex = TStackPosition.Unassigned then exit;
    Container.InBay := LoadingBay.Identifer;
    Inc(LoadingBay.ContainerCount, 1);
    Container.BayPosition := StackIndex;
    LoadingBay.Stack[Integer(StackIndex)] := Container.Identifer;
end;

{ Procedure: CraneHold
  Purpose: Sets the container the crane is holding.
  Parameters:
    [out] Crane      (TCrane)      - The crane.
    [out] Container  (TContainer)  - The container the crane will hold.
    [out] LoadingBay (TLoadingBay) - The loading bay the container is in before.
}
procedure CraneHold(
    var Crane: TCrane; var Container: TContainer; var LoadingBay: TLoadingBay);
begin
    Crane.Holding := Container.Identifer;
    LoadingBay.Stack[Integer(Container.BayPosition)] := 0;
    Container.InBay := '0';
    Dec(LoadingBay.ContainerCount, 1);
end;

{ Procedure: Path
  Purpose: Adds the path to the stored array.
  Parameters:
    [out] Paths          (Array of TPath) - The array of paths to be updated.
    [out] PathCount      (Integer)        - The path count to be updated.
    [in]  LoadingBayFrom (TLoadingBay)    - The path from the specified loading bay.
    [in]  LoadingBayTo   (TloadingBay)    - The path to the specified loading bay.
}
procedure Path(
    var Paths: array of TPath; var PathCount: Integer; 
    LoadingBayFrom, LoadingBayTo: TLoadingBay);
begin
    Paths[PathCount].LFrom := LoadingBayFrom.Identifer;
    Paths[PathCount].LTo := LoadingBayTo.Identifer;
    Inc(PathCount, 1);
end;

{ Function: MoveCost
  Purpose: Calculates the cost of the crane moving between loading bays.
  Parameters:
    [in] LoadingBayFrom (TLoadingBay) - The loading bay to move from.
    [in] LoadingBayTo   (TLoadingBay) - The loading bay to move to.
  Returns:
    Result (Integer) - The move cost.
}
function MoveCost(LoadingBayFrom, LoadingBayTo: TLoadingBay): Integer;
var Distance: Integer;
begin
    Distance := Abs(Ord(LoadingBayFrom.Identifer) - Ord(LoadingBayTo.Identifer));
    MoveCost := (LOADING_BAY_TIME * Distance);
end;

{ Function: PickUpCost
  Purpose: Calculates the cost of the picking up a container.
  Parameters:
    [in] Pos (TStackPosition) - The position of the container in a loading bay.
  Returns:
    Result (Integer) - The pick up cost.
}
function PickUpCost(Pos: TStackPosition): Integer;
begin
    PickUpCost := MAX_PICKUP_TIME - Integer(Pos);
end;

{ Function: DropCost
  Purpose: Returns the cost of dropping a container.
  Parameters:
    N/A
  Returns:
    Result (Integer) - The drop cost.
}
function DropCost(): Integer;
begin DropCost := DROP_TIME end;

{ Function: Move
  Purpose: Moves the crane from loading bay to another.
  Parameters:
    [out] Crane          (TCrane)      - The crane.
    [in]  LoadingBayFrom (TLoadingBay) - The loading bay the crane is over.
    [in]  LoadingBayTo   (TLoadingBay) - The loading bay the crane should move to.
  Returns:
    Result (Integer) - The move cost.
}
function Move(
    var Crane: TCrane; LoadingBayFrom, LoadingBayTo: TLoadingBay): Integer;
begin
    Move := 0;
    if (Crane.AtBay <> LoadingBayFrom.Identifer) 
        and (Crane.AtBay <> LoadingBayTo.Identifer) then exit;
    Crane.AtBay := LoadingBayTo.Identifer;
    Move := MoveCost(LoadingBayFrom, LoadingBayTo);
end;

{ Function: PickUp
  Purpose: The crane picks up a container.
  Parameters:
    [out] Crane      (TCrane)              - The crane.
    [out] LoadingBay (TLoadingBay)         - The loading bay the crane is over.
    [out] Containers (Array of TContainer) - The array of all containers.
  Returns:
    Result (Integer) - The pickup cost.
}
function PickUp(
    var Crane: TCrane; var LoadingBay: TLoadingBay;
    var Containers: array of TContainer): Integer;
begin
    PickUp := 0;
    if Crane.AtBay <> LoadingBay.Identifer then exit;
    if Crane.Holding <> 0 then exit;
    if LoadingBay.ContainerCount = 0 then exit;
    Crane.Holding := LoadingBay.Stack[LoadingBay.ContainerCount-1];
    LoadingBay.Stack[LoadingBay.ContainerCount-1] := 0;
    Dec(LoadingBay.ContainerCount, 1);
    PickUp := PickUpCost(Containers[Crane.Holding-1].BayPosition);
    Containers[Crane.Holding-1].BayPosition := TStackPosition.Unassigned;
    Containers[Crane.Holding-1].PickUpByCrane := True;
    Containers[Crane.Holding-1].InBay := '0';
end;

{ Function: Drop
  Purpose: The crane drops the container it is holding.
  Parameters:
    [out] Crane      (TCrane)              - The crane.
    [out] LoadingBay (TLoadingBay)         - The loading bay the crane is over.
    [out] Containers (Array of TContainer) - The array of all containers.
  Returns:
    Result (Integer) - The drop cost.
}
function Drop(
    var Crane: TCrane; var LoadingBay: TLoadingBay;
    var Containers: array of TContainer): Integer;
begin
    Drop := 0;
    if Crane.AtBay <> LoadingBay.Identifer then exit;
    if Crane.Holding = 0 then exit;
    if LoadingBay.ContainerCount = 3 then exit;
    Containers[Crane.Holding-1].BayPosition := TStackPosition(LoadingBay.ContainerCount);
    Containers[Crane.Holding-1].PickUpByCrane := False;
    Containers[Crane.Holding-1].InBay := LoadingBay.Identifer;
    LoadingBay.Stack[LoadingBay.ContainerCount] := Crane.Holding;
    Inc(LoadingBay.ContainerCount, 1);
    Crane.Holding := 0;
    Drop := DropCost();
end;

{ Function: HeurBayDistance
  Purpose: Calculates the heuristic - distance between bays.
  Parameters:
    [in] Bay            (Char) - The target loading bay.
    [in] ContainerInBay (Char) - The current loading bay.
}
function HeurBayDistance(Bay: Char; ContainerInBay: Char): Integer;
begin
    HeurBayDistance := Abs(3 * (Ord(Bay) - Ord(ContainerInBay)));
end;

{ Function: HeurIndexDistance
  Purpose: Calculates the heuristic - distance between indexes
  Parameters:
    [in] Container (TContainer)     - The container.
    [in] Bay       (Char)           - The target loading bay.
    [in] Position  (TStackPosition) - The target stack position of the container.
    [in] Crane     (TCrane)         - The crane.
}
function HeurIndexDistance(
    Container: TContainer; Bay: Char; Position: TStackPosition;
    Crane: TCrane): Integer;
begin
    HeurIndexDistance := 0;
    if Container.InBay = Bay then
    begin
        if Container.BayPosition = Position then exit;
        Inc(HeurIndexDistance, Abs(Integer(Container.BayPosition) - Integer(Position)));
    end
    else if Container.InBay = '0' then
    begin
        Inc(HeurIndexDistance, HeurBayDistance(Bay, Crane.AtBay));
        Inc(HeurIndexDistance, PickUpCost(Position));
    end
    else
    begin
        Inc(HeurIndexDistance, PickUpCost(Container.BayPosition));
        Inc(HeurIndexDistance, PickUpCost(Position));
    end;
end;

{ Function: Heur
  Purpose: Calculates the heuristic
  Parameters:
    [in] Env (TEnvironment) - The environment.
}
function Heur(Env: TEnvironment): Integer;
var 
    Index: Integer;
    Dist: array [0..5] of Integer;
begin 
    Heur := 0;
    Dist[0] := HeurBayDistance('A', Env.Containers[0].InBay);
    Dist[1] := HeurBayDistance('A', Env.Containers[1].InBay);
    Dist[2] := HeurBayDistance('A', Env.Containers[2].InBay);
    Dist[3] := HeurBayDistance('B', Env.Containers[3].InBay);
    Dist[4] := HeurBayDistance('B', Env.Containers[4].InBay);

    for Index := 0 to CONTAINER_COUNT-1 do
        if Dist[Index] > 30 then Dist[Index] := 0;
    
    Inc(Dist[0], HeurIndexDistance(
        Env.Containers[0], 'A', TStackPosition.Bottom, Env.Crane));
    Inc(Dist[1], HeurIndexDistance(
        Env.Containers[1], 'A', TStackPosition.Middle, Env.Crane));
    Inc(Dist[2], HeurIndexDistance(
        Env.Containers[2], 'A', TStackPosition.Top, Env.Crane));
    Inc(Dist[3], HeurIndexDistance(
        Env.Containers[3], 'B', TStackPosition.Bottom, Env.Crane));
    Inc(Dist[4], HeurIndexDistance(
        Env.Containers[4], 'B', TStackPosition.Middle, Env.Crane));

    Inc(Heur, (Dist[0] + Dist[1] + Dist[2] + Dist[3] + Dist[4]));
end;

{ Procedure: DisplayGraphic
  Purpose: Display the environment as a text based graphic.
  Paramters:
    [in] Env (TEnvironment) - The whole environment.
}
procedure DisplayGraphic(Env: TEnvironment);
var Index, StackIndex, CraneIndex: Integer;
begin
    CraneIndex := Ord(Env.Crane.AtBay) - 65;
    WriteLn();
    for Index := 0 to (LOADING_BAY_COUNT-1) do
    begin
        if Index <> CraneIndex then begin
            Write('   '); continue; end;
        if Env.Crane.Holding = 0 then continue;
        Write(Format(' %d ', [Env.Crane.Holding]));
    end;
    WriteLn();
    for Index := 0 to (LOADING_BAY_COUNT-1) do
        if Index = CraneIndex then Write(' ! ') else Write('   ');
    WriteLn();
    WriteLn();
    for StackIndex := (CONTAINER_STACK_LIMIT-1) downto 0 do
    begin
        for Index := 0 to (LOADING_BAY_COUNT-1) do
            Write(Format(
                ' %d ', [Env.LoadingBays[Index].Stack[StackIndex]]));
        WriteLn();
    end;
    for Index := 0 to (LOADING_BAY_COUNT-1) do
        Write(Format(' %s ', [Env.LoadingBays[Index].Identifer]));
    WriteLn();
    WriteLn();
    WriteLn(Format('Cost: %d', [Env.TotalCost]));
    WriteLn(Format('Heur: %d', [Heur(Env)]));
    WriteLn(Format('f(x): %d', [Heur(Env) + Env.TotalCost]));
    WriteLn();
end;


var
    Env: TEnvironment;
    LoadingBayA, LoadingBayB, LoadingBayC, LoadingBayD: ^TLoadingBay;
    Container1, Container2, Container3, Container4, Container5: ^TContainer;
    Crane: ^TCrane;
    Paths: ^TPath;
    TotalCost: ^Integer;
    ErrorOcurred: Boolean;

begin
    // Pointers used to easily refer to parts of the environment;
    LoadingBayA := @(Env.LoadingBays[0]);
    LoadingBayB := @(Env.LoadingBays[1]);
    LoadingBayC := @(Env.LoadingBays[2]);
    LoadingBayD := @(Env.LoadingBays[3]);
    Container1 := @(Env.Containers[0]);
    Container2 := @(Env.Containers[1]);
    Container3 := @(Env.Containers[2]);
    Container4 := @(Env.Containers[3]);
    Container5 := @(Env.Containers[4]);
    Crane := @(Env.Crane);
    Paths := @(Env.Paths);
    TotalCost := @(Env.TotalCost);
    
    // Initialise structures, default values.
    // This includes identifiers for the loading bays and containers.
    LoadingBayInit(LoadingBayA^, 'A');
    LoadingBayInit(LoadingBayB^, 'B');
    LoadingBayInit(LoadingBayC^, 'C');
    LoadingBayInit(LoadingBayD^, 'D');
    ContainerInit(Container1^, 1);
    ContainerInit(Container2^, 2);
    ContainerInit(Container3^, 3);
    ContainerInit(Container4^, 4);
    ContainerInit(Container5^, 5);
    CraneInit(Crane^);
    PathsInit(Paths^);

    // Initial State.
    // Setting positions of containers and the crane.
    // Setting and checking the costs.
    CraneAt(Crane^, LoadingBayD^);
    ContainerAt(Container1^, LoadingBayA^, TStackPosition.Bottom);
    ContainerAt(Container2^, LoadingBayB^, TStackPosition.Bottom);
    ContainerAt(Container3^, LoadingBayC^, TStackPosition.Bottom);
    ContainerAt(Container4^, LoadingBayC^, TStackPosition.Middle);
    ContainerAt(Container5^, LoadingBayA^, TStackPosition.Middle);
    Path(Paths^, Env.PathCount, LoadingBayA^, LoadingBayB^);
    Path(Paths^, Env.PathCount, LoadingBayB^, LoadingBayC^);
    Path(Paths^, Env.PathCount, LoadingBayC^, LoadingBayD^);
    Assert(MoveCost(LoadingBayA^, LoadingBayB^) = 3);
    Assert(MoveCost(LoadingBayB^, LoadingBayA^) = 3);
    Assert(MoveCost(LoadingBayB^, LoadingBayC^) = 3);
    Assert(MoveCost(LoadingBayC^, LoadingBayB^) = 3);
    Assert(MoveCost(LoadingBayC^, LoadingBayD^) = 3);
    Assert(MoveCost(LoadingBayD^, LoadingBayC^) = 3);
    Assert(PickUpCost(TStackPosition.Bottom) = 4);
    Assert(PickUpCost(TStackPosition.Middle) = 3);
    Assert(PickUpCost(TStackPosition.Top) = 2);
    Assert(DropCost() = 2);
    TotalCost^ := 0;

    // Plan - The steps to get to the goal state.
    Inc(TotalCost^, Move(Crane^, LoadingBayD^, LoadingBayC^));
    Inc(TotalCost^, Move(Crane^, LoadingBayC^, LoadingBayB^));
    Inc(TotalCost^, Move(Crane^, LoadingBayB^, LoadingBayA^));
    Inc(TotalCost^, PickUp(Crane^, LoadingBayA^, Env.Containers));
    Inc(TotalCost^, Move(Crane^, LoadingBayA^, LoadingBayB^));
    Inc(TotalCost^, Move(Crane^, LoadingBayB^, LoadingBayC^));
    Inc(TotalCost^, Move(Crane^, LoadingBayC^, LoadingBayD^));
    Inc(TotalCost^, Drop(Crane^, LoadingBayD^, Env.Containers));
    Inc(TotalCost^, Move(Crane^, LoadingBayD^, LoadingBayC^));
    Inc(TotalCost^, Move(Crane^, LoadingBayC^, LoadingBayB^));
    Inc(TotalCost^, PickUp(Crane^, LoadingBayB^, Env.Containers));
    Inc(TotalCost^, Move(Crane^, LoadingBayB^, LoadingBayA^));
    Inc(TotalCost^, Drop(Crane^, LoadingBayA^, Env.Containers));
    Inc(TotalCost^, Move(Crane^, LoadingBayA^, LoadingBayB^));
    Inc(TotalCost^, Move(Crane^, LoadingBayB^, LoadingBayC^));
    Inc(TotalCost^, PickUp(Crane^, LoadingBayC^, Env.Containers));
    Inc(TotalCost^, Move(Crane^, LoadingBayC^, LoadingBayB^));
    Inc(TotalCost^, Drop(Crane^, LoadingBayB^, Env.Containers));
    Inc(TotalCost^, Move(Crane^, LoadingBayB^, LoadingBayC^));
    Inc(TotalCost^, PickUp(Crane^, LoadingBayC^, Env.Containers));
    Inc(TotalCost^, Move(Crane^, LoadingBayC^, LoadingBayB^));
    Inc(TotalCost^, Move(Crane^, LoadingBayB^, LoadingBayA^));
    Inc(TotalCost^, Drop(Crane^, LoadingBayA^, Env.Containers));
    Inc(TotalCost^, Move(Crane^, LoadingBayA^, LoadingBayB^));
    Inc(TotalCost^, Move(Crane^, LoadingBayB^, LoadingBayC^));
    Inc(TotalCost^, Move(Crane^, LoadingBayC^, LoadingBayD^));
    Inc(TotalCost^, PickUp(Crane^, LoadingBayD^, Env.Containers));
    Inc(TotalCost^, Move(Crane^, LoadingBayD^, LoadingBayC^));
    Inc(TotalCost^, Move(Crane^, LoadingBayC^, LoadingBayB^));
    Inc(TotalCost^, Drop(Crane^, LoadingBayB^, Env.Containers));

    // Displaying Graphic.
    DisplayGraphic(Env);

    // Goal State Check.
    // Container 1 @ Loading Bay A @ Bottom;
    // Container 2 @ Loading Bay A @ Middle;
    // Container 3 @ Loading Bay A @ Top;
    // Container 4 @ Loading Bay B @ Bottom;
    // Container 5 @ Loading Bay B @ Middle;
    ErrorOcurred := False;
    try
        Assert(
            (Container1^.InBay = LoadingBayA^.Identifer) and 
            (Container1^.BayPosition = TStackPosition.Bottom));
        Assert(
            (Container2^.InBay = LoadingBayA^.Identifer) and 
            (Container2^.BayPosition = TStackPosition.Middle));
        Assert(
            (Container3^.InBay = LoadingBayA^.Identifer) and 
            (Container3^.BayPosition = TStackPosition.Top));
        Assert(
            (Container4^.InBay = LoadingBayB^.Identifer) and 
            (Container4^.BayPosition = TStackPosition.Bottom));
        Assert(
            (Container5^.InBay = LoadingBayB^.Identifer) and 
            (Container5^.BayPosition = TStackPosition.Middle));
    except
        on E: EAssertionFailed do 
        begin
            WriteLn(StdErr, 'Goal state has not been met.');
            ErrorOcurred := True; 
        end;
    end;

    if not ErrorOcurred then WriteLn('Goal state has been met.')
end.