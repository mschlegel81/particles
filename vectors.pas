UNIT vectors;

{$mode objfpc}{$H+}

INTERFACE
USES GL;
TYPE
  TVector3=array[0..2] of GLfloat;

FUNCTION vectorOf(CONST x,y,z:single):TVector3;
FUNCTION randomOnSphere:TVector3;
OPERATOR * (CONST x:TVector3; CONST y:double):TVector3;
OPERATOR +(CONST x,y:TVector3):TVector3;
OPERATOR -(CONST x,y:TVector3):TVector3;
FUNCTION euklideanNorm(CONST x:TVector3):double;
FUNCTION hsvColor(h,s,v:single):TVector3;

CONST ZERO_VECTOR:TVector3=(0,0,0);
IMPLEMENTATION
USES math;
FUNCTION vectorOf(CONST x,y,z:single):TVector3;
  begin
    result[0]:=x;
    result[1]:=y;
    result[2]:=z;
  end;

FUNCTION randomOnSphere:TVector3;
  VAR n:double;
  begin
    repeat
      result[0]:=2*random-1;
      result[1]:=2*random-1;
      result[2]:=2*random-1;
      n:=euklideanNorm(result);
    until (n<1) and (n>1E-2);
    result*=1/n;
  end;

OPERATOR * (CONST x:TVector3; CONST y:double):TVector3;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
    result[2]:=x[2]*y;
  end;

OPERATOR +(CONST x,y:TVector3):TVector3;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
    result[2]:=x[2]+y[2];
  end;

OPERATOR -(CONST x,y:TVector3):TVector3;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
    result[2]:=x[2]-y[2];
  end;

FUNCTION euklideanNorm(CONST x:TVector3):double;
  begin
    result:=sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
  end;

FUNCTION hsvColor(h,s,v:single):TVector3;
  VAR hi:byte;
      p,q,t:single;
  begin
    initialize(result);
    result[0]:=0;
    result[1]:=0;
    result[2]:=0;
    if isInfinite(h) or isNan(h) then exit(result);
    if h>1 then h:=frac(h)
    else if h<0 then h:=1+frac(h);

    while h<0 do h:=h+1;
    while h>1 do h:=h-1;

    hi:=trunc(h*6); h:=h*6-hi;
    p:=v*(1-s      );
    q:=v*(1-s*   h );
    t:=v*(1-s*(1-h));
    case hi of
      1  : result:=vectorOf(q,v,p);
      2  : result:=vectorOf(p,v,t);
      3  : result:=vectorOf(p,q,v);
      4  : result:=vectorOf(t,p,v);
      5  : result:=vectorOf(v,p,q);
      else result:=vectorOf(v,t,p);
    end;
  end;

FUNCTION extractHsvChannels(CONST x:TVector3):TVector3;
  VAR brightChannel:byte;
  begin
    initialize(result);
    if x[0]>x[1]      then begin result[2]:=x[0]; brightChannel:=0; end
                      else begin result[2]:=x[1]; brightChannel:=1; end;
    if x[2]>result[2] then begin result[2]:=x[2]; brightChannel:=2; end;
    //result[2] now holds the brightest component of x
    if x[0]<x[1]      then result[1]:=x[0]
                      else result[1]:=x[1];
    if x[2]<result[1] then result[1]:=x[2];
    //result[1] now holds the darkest component of x
    case brightChannel of
      0 : result[0]:=(  (x[1]-x[2])/(result[2]-result[1]))/6;
      1 : result[0]:=(2+(x[2]-x[0])/(result[2]-result[1]))/6;
      2 : result[0]:=(4+(x[0]-x[1])/(result[2]-result[1]))/6;
    end;
    result[1]:=(result[2]-result[1])/result[2];
    while result[0]<0 do result[0]:=result[0]+1;
    while result[0]>1 do result[0]:=result[0]-1;
  end;

end.

