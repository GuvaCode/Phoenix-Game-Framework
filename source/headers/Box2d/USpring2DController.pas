unit USpring2DController;

{ box2D 2.3.0 }



interface
{$I Physics2D.inc}


uses
   UPhysics2DTypes, UPhysics2D;

type

 TSpringController = class(Tb2Controller)
    springConstant: physicsfloat;
    damping: physicsfloat;
    restlength: physicsfloat;
    a, b: Tb2Body;
    onVar: boolean;
    function isOn: boolean;
    function isOff: boolean;
    function getOneEnd: Tb2Body;
    function getTheOtherEnd: Tb2Body;
    function currentLength: physicsfloat;
    function getRestLength: physicsfloat;
    function strength: physicsfloat;
    function getDamping: physicsfloat;
    procedure turnOff;
    procedure turnOn;
    procedure setStrength(ks: physicsfloat);
    procedure setDamping(d: physicsfloat);
    procedure setRestLength(l: physicsfloat);
    procedure Step(const step: Tb2TimeStep); override;
    constructor Create(Ap, Bp: Tb2Body; ks, d, r: physicsfloat);

end;

implementation


constructor TSpringController.Create(Ap, Bp: Tb2Body; ks, d, r: physicsfloat);
begin
    springConstant := ks;
    damping := d;
    restLength := r;
    a := Ap;
    b := Bp;
    onVar := true;
end;

function TSpringController.isOn: boolean;
begin
  result := onVar;
end;

function TSpringController.isOff: boolean;
begin
  result := not onVar;
end;

function TSpringController.getOneEnd: Tb2Body;
begin
  result := a;
end;

function TSpringController.getTheOtherEnd: Tb2Body;
begin
  result := b;
end;

function TSpringController.currentLength: physicsfloat;
var
pp1, pp2: TVector2;
d: TVector2;
begin
  pp1 := a.GetPosition;
  pp2 := b.GetPosition;
  {$IFDEF OP_OVERLOAD}
    d := pp2 - pp1;
    result := d.Length;
  {$ELSE}
    Result := LengthVec(Subtract(pp1,pp2));
  {$ENDIF}
end;

function TSpringController.getRestLength: physicsfloat;
begin
  result := restLength;
end;

function TSpringController.strength: physicsfloat;
begin
  result := springConstant;
end;

function TSpringController.getDamping: physicsfloat;
begin
  result := damping;
end;

procedure TSpringController.turnOff;
begin
  onVar := false;
end;

procedure TSpringController.turnOn;
begin
  onVar := true;
end;

procedure TSpringController.setStrength(ks: physicsfloat);
begin
  springConstant := ks;
end;

procedure TSpringController.setDamping(d: physicsfloat);
begin
  damping := d;
end;

procedure TSpringController.setRestLength(l: physicsfloat);
begin
  restLength := l;
end;

procedure TSpringController.Step(const step: Tb2TimeStep);
var
a2bX, a2bY, a2bZ: physicsfloat;
a2bDistance: physicsfloat;
springForce: physicsfloat;
Va2bx, Va2by, Va2bz: physicsfloat;
dampingForce: physicsfloat;
r: physicsfloat;
f1, f2: TVector2;
begin
//  If onVar and (a.isFree or b.isFree) then
    if OnVar then
    begin
      a2bX := a.Getposition.x - b.Getposition.x;
		  a2bY := a.Getposition.y - b.Getposition.y;
		  a2bZ := 0;
      a2bDistance := sqrt(a2bX*a2bX + a2bY*a2bY + a2bZ*a2bZ);
      if a2bDistance = 0 then
        begin
    			a2bX := 0;
		    	a2bY := 0;
    			a2bZ := 0;
        end else
        begin
			    a2bX := a2bx/a2bDistance;
			    a2bY := a2bY/a2bDistance;
			    a2bZ := a2bZ/a2bDistance;
        end;

		// spring force is proportional to how much it stretched

      springForce := -(a2bDistance - restLength) * springConstant;

 		// want velocity along line b/w a & b, damping force is proportional to this

      Va2bX := a.GetLinearvelocity.x - b.GetLinearvelocity.x;
      Va2bY := a.Getlinearvelocity.y - b.GetLinearvelocity.y;
	 //	  Va2bZ := a.velocity.z - b.velocity.z;
      Va2bz := 0;
      dampingForce := -damping * (a2bX*Va2bX + a2bY*Va2bY + a2bZ*Va2bZ);

		// forceB is same as forceA in opposite direction

      r := springForce + dampingForce;

		  a2bX := a2bX * r;
		  a2bY := a2bY * r;
		  a2bZ := a2bZ * r;
      f1.x := a2bx;
      f1.y := a2by;
      f2.x := -a2bx;
      f2.y := -a2by;

   //   if a.isFree then
     a.applyforce(f1,b.GetWorldPoint(b2Vec2_zero),true );

		 // if b.isFree then
			 //   b.applyForce(-a2bX, -a2bY);
        b.applyforce(f2,a.GetWorldPoint(b2Vec2_zero),true );
     end;
end;

end.
