program DockingProblem;

{$mode objfpc}
{$apptype console}
{$assertions on}

uses SysUtils;

const
    // Limits
    CONTAINER_COUNT = 5;
    LOADING_BAY_COUNT = 4;
    CONTAINER_STACK_LIMIT = 3;
    PATH_ALLOC_LIMIT = 50;

    // Time In Minutes
    LOADING_BAY_TIME = 3;
    MAX_PICKUP_TIME = 4;
    DROP_TIME = 2;

type
    TStackPosition = (Unassigned = -1, Bottom = 0, Middle = 1, Top = 2);

    TContainer = record
        Identifer: Integer;
        InBay: Char;
        BayPosition: TStackPosition;
        PickUpByCrane: Boolean;
    end;

    TLoadingBay = record
        Identifer: Char;
        Stack: array [0..CONTAINER_STACK_LIMIT] of Integer;
        ContainerCount: Integer;
    end;

    TCrane = record
        AtBay: Char;
        Holding: Integer;
    end;

    TPath = record
        LFrom: Char;
        LTo: Char;
    end;

    TEnvironment = record
        LoadingBays: array [0..LOADING_BAY_COUNT] of TLoadingBay;
        Containers: array [0..CONTAINER_COUNT] of TContainer;
        Paths: array [0..PATH_ALLOC_LIMIT] of TPath;
        PathCount: Integer;
        Crane: TCrane;
        TotalCost: Integer;
    end;

procedure LoadingBayInit(var LoadingBay: TLoadingBay; Identifer: Char);
var Index: Integer;
begin
    LoadingBay.Identifer := Identifer;
    LoadingBay.ContainerCount := 0;
    for Index := 0 to (CONTAINER_STACK_LIMIT-1) do
        LoadingBay.Stack[Index] := 0;
end;

procedure CraneInit(var Crane: TCrane);
begin
    Crane.AtBay := '0';
    Crane.Holding := 0;
end;

procedure ContainerInit(var Container: TContainer; Identifer: Integer);
begin
    Container.Identifer := Identifer;
    Container.BayPosition := TStackPosition.Unassigned;
end;

procedure PathsInit(var Paths: array of TPath);
var Index: Integer;
begin
    for Index := 0 to PATH_ALLOC_LIMIT do
    begin 
        Paths[Index].LFrom := Char('0');
        Paths[Index].LTo := Char('0');
    end;
end;

procedure CraneAt(var Crane: TCrane; LoadingBay: TLoadingBay);
begin
    Crane.AtBay := LoadingBay.Identifer;
end;

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

procedure CraneHold(
    var Crane: TCrane; var Container: TContainer; var LoadingBay: TLoadingBay);
begin
    Crane.Holding := Container.Identifer;
    LoadingBay.Stack[Integer(Container.BayPosition)] := 0;
    Container.InBay := '0';
    Dec(LoadingBay.ContainerCount, 1);
end;

procedure Path(
    var Paths: array of TPath; var PathCount: Integer; 
    LoadingBayFrom, LoadingBayTo: TLoadingBay);
begin
    Paths[PathCount].LFrom := LoadingBayFrom.Identifer;
    Paths[PathCount].LTo := LoadingBayTo.Identifer;
    Inc(PathCount, 1);
end;

function MoveCost(LoadingBayFrom, LoadingBayTo: TLoadingBay): Integer;
var Distance: Integer;
begin
    Distance := Abs(Ord(LoadingBayFrom.Identifer) - Ord(LoadingBayTo.Identifer));
    MoveCost := (LOADING_BAY_TIME * Distance);
end;

function PickUpCost(Pos: TStackPosition): Integer;
begin
    PickUpCost := MAX_PICKUP_TIME - Integer(Pos);
end;

function DropCost(): Integer;
begin DropCost := DROP_TIME end;

function Move(
    var Crane: TCrane; LoadingBayFrom, LoadingBayTo: TLoadingBay): Integer;
begin
    Move := 0;
    if (Crane.AtBay <> LoadingBayFrom.Identifer) 
        and (Crane.AtBay <> LoadingBayTo.Identifer) then exit;
    Crane.AtBay := LoadingBayTo.Identifer;
    Move := MoveCost(LoadingBayFrom, LoadingBayTo);
end;

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

function HeurBayDistance(Bay: Char; ContainerInBay: Char): Integer;
begin
    HeurBayDistance := Abs(3 * (Ord(Bay) - Ord(ContainerInBay)));
end;

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

    // Plan.   
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