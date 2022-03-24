UNIT particlePhysics;

{$mode objfpc}{$H+}

INTERFACE
USES vectors,GL,serializationUtil;

TYPE
  TParticle = record
    p,v,a,
    color:TVector3;
  end;

  FUpdateAcceleration              =PROCEDURE(CONST progress:double) of object;
  FUpdateAccelerationNoninteracting=PROCEDURE(CONST progress:double; CONST particleIndex:longint) of object;

  { TParticleEngine }

  TParticleEngine = class
  private
    //Pseudo constants
    spherePoints: array [0..1023] of TVector3;
    colorDelta  : array [0..1023] of TVector3;
    //State
    lastModeTicks:longint;
    attractionMode:byte;
    commonTargetColor:TVector3;
    commonSaturation,commonHueOffset:TGLfloat;
    lissajousParam:array[0..2] of byte;
    Particle:  array [0..1023] of TParticle;
    gridPoint: array [0..1023] of TIntVec3;

    PROCEDURE updateA_cyclic(CONST progress:double);
    PROCEDURE updateA_cyclicMirror(CONST progress:double);
    PROCEDURE updateA_groupedCyclic(CONST progress:double);

    PROCEDURE updateA_cube(CONST progress:double; CONST particleIndex:longint);
    PROCEDURE updateA_sphere(CONST progress:double; CONST particleIndex:longint);
    PROCEDURE updateA_swirl(CONST progress:double; CONST particleIndex:longint);
    PROCEDURE updateA_icosahedron(CONST progress:double; CONST particleIndex:longint);
    PROCEDURE updateA_wave(CONST progress:double; CONST particleIndex:longint);
    PROCEDURE calculateGridPositions;
    PROCEDURE updateA_grid(CONST progress:double; CONST particleIndex:longint);
    PROCEDURE updateA_lissajous(CONST progress:double; CONST particleIndex:longint);

    PROCEDURE updateA_heart(CONST progress:double; CONST particleIndex:longint);

    PROCEDURE updateA_vogler(CONST progress:double; CONST particleIndex:longint);
    PROCEDURE updateCol_vogler(CONST progress,dt: double);
    PROCEDURE updateA_clock(CONST progress:double; CONST particleIndex:longint);
    PROCEDURE updateColors_clock(CONST dt:double);

    PROCEDURE updateA_CIRCL(CONST progress:double);
    PROCEDURE updateA_clusters(CONST progress:double);
    PROCEDURE updateA_sheet(CONST progress:double);
    PROCEDURE updateColors_sheet(CONST progress,dt:double);
    PROCEDURE updateA_bicyclic(CONST progress:double);

    PROCEDURE updateA_sliver(CONST progress: double);
    PROCEDURE updateA_byDistance(CONST progress: double);
    PROCEDURE lorenzAttractor(CONST dt:double);
    PROCEDURE thomasAttractor(CONST dt:double);

    PROCEDURE updateA_X1(CONST progress: double);
    PROCEDURE updateA_X2(CONST progress: double);

    PROCEDURE switchAttractionMode(CONST forcedMode:byte=255);
    PROCEDURE MoveParticles(CONST modeTicks:longint);

    FUNCTION capSubSteps(CONST proposedSubSteps:double; CONST otherProposedSubSteps:double=0):longint;
  public
    lockCurrentSetup:boolean;
    MODE_SWITCH_INTERVAL_IN_TICKS:longint;
    TICKS_PER_SIMULATION_TIME_UNIT:double;

    CONSTRUCTOR create;
    DESTRUCTOR destroy; override;
    FUNCTION update(VAR modeTicks:longint):single;
    PROCEDURE nextSetup(VAR modeTicks:longint; CONST forcedMode:byte=255);
    PROCEDURE DrawParticles(CONST ParticleList: GLuint; CONST particleRotX,particleRotY:GLfloat);
    PROPERTY currentAttractionMode:byte read attractionMode;

    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

CONST
  GRID_TARGET=6;
  ICOSAHEDRON_TARGET=15;
  CLOCK_TARGET=20;
  PYRAMID_TARGET=25;
  FIREWORKS_TARGET=26;

  C_IcosahedronNodes:array[0..11] of TVector3=(
  ( 0, 8.50650808352040E-001, 5.25731112119134E-001),
  ( 5.25731112119134E-001, 0, 8.50650808352040E-001),
  ( 8.50650808352040E-001, 5.25731112119134E-001, 0),
  (-5.25731112119134E-001, 0, 8.50650808352040E-001),
  (-8.50650808352040E-001, 5.25731112119134E-001, 0),
  ( 0,-8.50650808352040E-001, 5.25731112119134E-001),
  ( 8.50650808352040E-001,-5.25731112119134E-001, 0),
  (-8.50650808352040E-001,-5.25731112119134E-001, 0),
  ( 0, 8.50650808352040E-001,-5.25731112119134E-001),
  ( 5.25731112119134E-001, 0,-8.50650808352040E-001),
  (-5.25731112119134E-001, 0,-8.50650808352040E-001),
  ( 0,-8.50650808352040E-001,-5.25731112119134E-001));
  ATTRACTION_MODE_COUNT=27;

  ATTRACTION_MODE_NAME:array[0..ATTRACTION_MODE_COUNT-1] of string=
  ('Cyclic', //0
   'Cube'  , //1
   'Sphere', //2
   'Heart' , //3
   'Wave'  , //4
   'Swirl' , //5
   'Grid'  , //6
   'Rain'  , //7
   'Lissajous', //8
   'Clusters', //9
   'Flower', //10
   'Grouped Lissajous', //11
   'Sliver', //12
   'Sheet', //13
   'Fountain', //14
   'Icosahedron', //15
   'Grouped Cyclic', //16
   'Lorenz Attractor', //17
   'Bi-Cyclic', //18
   'By Distance', //19
   'Clock', //20
   'Thomas Attractor', //21
   'Knot 1', //22
   'Knot 2', //23
   'Mirrored Cyclic', //24
   'Pyramid',
   'Fireworks');

IMPLEMENTATION
USES math,sysutils,LCLProc;
CONST
  WHITE:TVector3=(1,1,1);
  FIBFAK=2*pi/sqr((sqrt(5)-1)/2);

FUNCTION accel(CONST v,p,target:TVector3; CONST springConstant,dampingFctor:double):TVector3;
  begin result:=(target-p)*springConstant+v*(euklideanNorm(v)*dampingFctor); end;

PROCEDURE TParticleEngine.MoveParticles(CONST modeTicks: longint);

  PROCEDURE fixBrokenPositions;
    FUNCTION anyInvalid(CONST v:TVector3):boolean; inline;
      begin
        result:=isNan(v[0]) or isInfinite(v[0])
             or isNan(v[1]) or isInfinite(v[1])
             or isNan(v[2]) or isInfinite(v[2]);
      end;
    VAR i:longint;
    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do
      if anyInvalid(p) or anyInvalid(v) then begin
        p:=spherePoints[i]*5;
        v:=ZERO_VECTOR;
        a:=ZERO_VECTOR;
        color:=WHITE*0.5+colorDelta[i];
      end;
    end;

  VAR dt:double;
      fallAndBounceDtSub:double;
      fallAndBounceSubSteps:longint;
  FUNCTION fallAndBounce(CONST i:longint; CONST speedDamingFactor:double=0.5):boolean;
    CONST BLUE  :TVector3=(0,0,1);
    VAR acc:TVector3;
        hitTime:double;
        subStep:longint;
    begin
      result:=false;
      for subStep:=1 to fallAndBounceSubSteps do with Particle[i] do begin
        acc[0]:=0;
        acc[1]:=-1;
        acc[2]:=0;
        v+=(acc-v*(euklideanNorm(v)*speedDamingFactor))*fallAndBounceDtSub;
        if p[1]<=-1 then begin
          p[1]:=-1;
          v[1]:=1;
          color:=BLUE;
          result:=true;
        end;
        if (v[1]<0) and (p[1]+v[1]*fallAndBounceDtSub<=-1) then begin
          hitTime:=(-1-p[1])/v[1];
          v[1]:=0.9*abs(v[1]);
          p[1]:=-1+v[1]*(fallAndBounceDtSub-hitTime);
          color:=color*0.7+BLUE*0.3;
          result:=true;
        end else p+=v*fallAndBounceDtSub;
      end;
    end;

  PROCEDURE moveTowardsTargets(CONST updateA:FUpdateAcceleration; CONST iMax_:longint=1023);
    VAR i,imax:longint;
        aMax:double=0;
        vMax:double=0;
        tmp,dtSub:double;
        subSteps,k:longint;
    begin
      if iMax_>=length(Particle)
      then imax:=length(Particle)-1
      else imax:=iMax_;

      updateA(min(1,lastModeTicks/MODE_SWITCH_INTERVAL_IN_TICKS));
      for i:=0 to imax do with Particle[i] do begin
        tmp:=vectors.sumOfSquares(a);
        if (tmp>aMax) and not(isInfinite(tmp)) and not(isNan(tmp)) then aMax:=tmp;
        tmp:=vectors.sumOfSquares(v);
        if (tmp>vMax) and not(isInfinite(tmp)) and not(isNan(tmp)) then vMax:=tmp;
      end;
      aMax:=sqrt(aMax);
      vMax:=sqrt(vMax);
      subSteps:=capSubSteps(dt*sqrt(aMax*500),
                            dt*     vMax*500);
      dtSub:=dt/subSteps;
      for k:=1 to subSteps do begin
        if k>1 then updateA(min(1,(lastModeTicks+dtSub*(k-1)*TICKS_PER_SIMULATION_TIME_UNIT)/MODE_SWITCH_INTERVAL_IN_TICKS));
        for i:=0 to imax do with Particle[i] do begin
          v+=a*dtSub;
          p+=v*dtSub;
        end;
      end;
      for i:=imax+1 to length(Particle)-1 do with Particle[i] do fallAndBounce(i);
    end;

  PROCEDURE moveTowardsTargetsNoninteracting(CONST updateA:FUpdateAccelerationNoninteracting; CONST iMax_:longint=1023);
    VAR i,imax:longint;
        dtSub:double;
        subSteps,k:longint;
        totalSubSteps:int64=0;
    begin
      if iMax_>=length(Particle)
      then imax:=length(Particle)-1
      else imax:=iMax_;
      for i:=0 to imax do with Particle[i] do begin
        updateA(min(1,lastModeTicks/MODE_SWITCH_INTERVAL_IN_TICKS),i);
        subSteps:=capSubSteps(dt*sqrt(euklideanNorm(a)*5000),
                              dt*     euklideanNorm(v)* 500);
        totalSubSteps+=subSteps;
        dtSub:=dt/subSteps;
        for k:=1 to subSteps do begin
          if k>1 then updateA(min(1,(lastModeTicks+dtSub*(k-1)*TICKS_PER_SIMULATION_TIME_UNIT)/MODE_SWITCH_INTERVAL_IN_TICKS),i);
          v+=a*dtSub;
          p+=v*dtSub;
        end;
      end;
      for i:=imax+1 to length(Particle)-1 do with Particle[i] do fallAndBounce(i);
    end;

  PROCEDURE updateColors_rainbow;
    VAR tgt:TVector3;
        i:longint;
    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        tgt:=hsvColor(i/length(Particle)+commonHueOffset,commonSaturation,1);
        color+=(tgt-color)*(0.1*dt);
      end;
    end;

  PROCEDURE updateColors_commmonTarget;
    VAR i:longint;

    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do
        color+=(commonTargetColor+colorDelta[i]-color)*(dt);
    end;

  PROCEDURE updateColors_grid;
    VAR i:longint;
        c:TVector3;
    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        c:=commonTargetColor+colorDelta[i];
        if gridPoint[i,lissajousParam[0] mod 3]=0 then c:=WHITE-c;
        color+=(c-color)*(0.2*dt);
      end;
    end;

  PROCEDURE updateColors_reds;
    begin
      commonTargetColor[2]-= commonTargetColor[2]*(0.3*dt);
      commonTargetColor[1]-= commonTargetColor[1]*(0.2*dt);
      commonTargetColor[0]-=(commonTargetColor[0]-0.5)*(0.1*dt);
      updateColors_commmonTarget;
    end;

  PROCEDURE updateColors_byVerticalVelocity;
    VAR tgt:TVector3;
        i:longint;
    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        if v[1]>0 then tgt:=commonTargetColor else tgt:=WHITE-commonTargetColor;
        color+=(tgt-color)*(dt);
      end;
    end;

  PROCEDURE updateColors_byRadius;
    VAR tgt:TVector3;
        i:longint;
    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        tgt:=hsvColor(euklideanNorm(p)+commonHueOffset,1,1);
        color+=(tgt-color)*(dt);
      end;
    end;

  PROCEDURE updateColors_byVelocity;
     VAR tgt:TVector3;
         vNorm:double;
         i:longint;
     begin
       for i:=0 to length(Particle)-1 do with Particle[i] do begin
         vNorm:=5*euklideanNorm(v);
         if vNorm<3 then begin
           tgt[0]:=vNorm;
           tgt[1]:=vNorm-1;
           tgt[2]:=vNorm-2;
         end else begin
           tgt[0]:=1-(vNorm-3)/(vNorm-2);
           tgt[1]:=tgt[0];
           tgt[2]:=1;
         end;
         color+=(tgt-color)*(2*dt);
       end;
     end;

  PROCEDURE fallingRain;
    VAR i:longint;
    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        fallAndBounce(i);
        if (p[1]<=-0.99) and (abs(v[1])<1E-2) then begin
          p[0]:=random+random+random+random+random+random-3;
          p[2]:=random+random+random+random+random+random-3;
          p[1]:=2;
          color:=commonTargetColor+(WHITE-commonTargetColor)*random;
          v:=ZERO_VECTOR;
          v[1]:=0.5-random;
        end;
      end;
    end;

  PROCEDURE fallingFountain;
    VAR i:longint;
    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        fallAndBounce(i);
        if (sqr(p[0])+sqr(p[1]+1)+sqr(p[2])<1E-2) then begin
          v[1]+=5;
          v+=randomOnSphere*0.2;
          color:=commonTargetColor+(WHITE-commonTargetColor)*random;
        end else if p[1]<=-0.99 then begin
           v[0]-=dt*p[0];
           v[2]-=dt*p[2];
        end;
      end;
    end;

  PROCEDURE fallingPyramid;
    VAR i,i0,i1:longint;
    begin
      i0:=round(length(Particle)*lastModeTicks*2/MODE_SWITCH_INTERVAL_IN_TICKS);
      i1:=round(length(Particle)*modeTicks    *2/MODE_SWITCH_INTERVAL_IN_TICKS);

      for i:=0 to length(Particle)-1 do with Particle[i] do begin

        if max(abs(p[2]),abs(p[0]))<=1 then begin
          p[1]+=max(abs(p[2]),abs(p[0]))-1;
          if fallAndBounce(i) then begin
            color:=WHITE-commonTargetColor;
            v*=0.1;
          end;
          p[1]-=max(abs(p[2]),abs(p[0]))-1;
        end else fallAndBounce(i);

        if (i>=i0) and (i<=i1) then begin
        //if (p[1]<=-0.99) and (abs(v[1])<1E-2) then begin
          p[0]:=1-2*random;
          p[2]:=1-2*random;
          if random<0.2 then begin
            if random<0.5
            then p[2]:=-p[0]
            else p[2]:=p[0];
          end else begin
            if abs(p[0])>abs(p[2])
            then p[0]:=0.1*round(10*p[0])
            else p[2]:=0.1*round(10*p[2]);
          end;
          p[1]:=2;
          color:=commonTargetColor+(WHITE-commonTargetColor)*random;
          v:=ZERO_VECTOR;
          v[1]:=0.5-random;
        end;
      end;
    end;

  PROCEDURE fireworks;
    CONST fallAcceleration:TVector3=(0,-1,0);
    VAR i,i0,group:longint;
        middle,sharedSpeed:TVector3;
        allDown:boolean=false;
        explodeHeight:double;
        setupTime:double;
    begin
      setupTime:=modeTicks/TICKS_PER_SIMULATION_TIME_UNIT;

      for group:=0 to length(Particle) shr 4 do begin

        explodeHeight:=group/(length(Particle) shr 5);

        i0:=group shl 5;

        sharedSpeed:=ZERO_VECTOR;
        allDown:=true;
        for i:=i0 to i0+31 do with Particle[i] do begin
          allDown:=allDown and (p[1]<-1);
          sharedSpeed+=v;
        end;

        if allDown and (setupTime-commonSaturation>0.5) then begin
          commonSaturation:=setupTime;
          middle:=randomInSphere;
          middle[1]:=-1;
          middle:=vectorOf(0,-1,0);

          sharedSpeed[0]:=2*pi*random;
          sharedSpeed[2]:=cos(sharedSpeed[0])*0.5;
          sharedSpeed[0]:=sin(sharedSpeed[1])*0.5;
          sharedSpeed[1]:=2;
          for i:=i0 to i0+31 do with Particle[i] do begin
            p:=middle;
            v:=sharedSpeed;
            color:=WHITE;
          end;
        end else if (Particle[i0].p[1]>=explodeHeight) and (euklideanNorm(Particle[i0].v-Particle[i0+8].v)<0.1) then begin
          middle:=hsvColor(random,1,2);
          for i:=0 to 31 do with Particle[i0+i] do begin
            v+=randomOnSphere*0.5;
            color:=middle;

          end;
        end;
      end;

      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        v+=fallAcceleration*dt;
        p+=v*dt;
        color-=color*(dt);
      end;

    end;

  begin
    fixBrokenPositions;
    dt:=(modeTicks-lastModeTicks)/TICKS_PER_SIMULATION_TIME_UNIT;
    fallAndBounceSubSteps:=capSubSteps(dt*dt*1000);
    fallAndBounceDtSub:=dt/fallAndBounceSubSteps;
    case attractionMode of
      0: begin
        moveTowardsTargets(@updateA_cyclic);
        updateColors_rainbow;
      end;
      1: begin
        moveTowardsTargetsNoninteracting(@updateA_cube,round(length(Particle)*modeTicks/MODE_SWITCH_INTERVAL_IN_TICKS*2));
        updateColors_commmonTarget;
      end;
      2: begin
        moveTowardsTargetsNoninteracting(@updateA_sphere,round(length(Particle)*modeTicks/MODE_SWITCH_INTERVAL_IN_TICKS*2));
        updateColors_commmonTarget;
      end;
      3: begin
        moveTowardsTargetsNoninteracting(@updateA_heart);
        updateColors_reds;
      end;
      4: begin
        moveTowardsTargetsNoninteracting(@updateA_wave);
        updateColors_byVerticalVelocity;
      end;
      5: begin
        moveTowardsTargetsNoninteracting(@updateA_swirl);
        updateColors_byVelocity;
      end;
      GRID_TARGET: begin
        moveTowardsTargetsNoninteracting(@updateA_grid);
        updateColors_grid;
      end;
      7: fallingRain;
      8: begin
        moveTowardsTargetsNoninteracting(@updateA_lissajous);
        updateColors_rainbow;
      end;
      9: begin
        moveTowardsTargets(@updateA_clusters);
        updateColors_byVelocity;
      end;
      10: begin
        moveTowardsTargetsNoninteracting(@updateA_vogler);
        updateCol_vogler(min(1,modeTicks/MODE_SWITCH_INTERVAL_IN_TICKS), dt);
      end;
      11: begin
        moveTowardsTargets(@updateA_CIRCL);
        updateColors_rainbow;
      end;
      12: begin
        moveTowardsTargets(@updateA_sliver,round(length(Particle)*modeTicks/MODE_SWITCH_INTERVAL_IN_TICKS*2));
        updateColors_byRadius;
      end;
      13: begin
        moveTowardsTargets(@updateA_sheet);
        updateColors_sheet(modeTicks/MODE_SWITCH_INTERVAL_IN_TICKS,dt);
      end;
      14: fallingFountain;
      ICOSAHEDRON_TARGET: begin
        moveTowardsTargetsNoninteracting(@updateA_icosahedron);//,round(length(Particle)*modeTicks/MODE_SWITCH_INTERVAL_IN_TICKS*2));
        updateColors_byRadius;
      end;
      16: begin
        moveTowardsTargets(@updateA_groupedCyclic);
        updateColors_byVelocity;
      end;
      17: lorenzAttractor(dt);
      18: begin
        moveTowardsTargets(@updateA_bicyclic);
        updateColors_byVerticalVelocity;
      end;
      19: begin
        moveTowardsTargets(@updateA_byDistance);
        //updateColors_byRadius;
        updateColors_rainbow;
      end;
      CLOCK_TARGET: begin
        moveTowardsTargetsNoninteracting(@updateA_clock);
        updateColors_clock(dt);
      end;
      21: thomasAttractor(dt);
      22: begin
        moveTowardsTargets(@updateA_X1);
        updateColors_byVelocity;
      end;
      23: begin
        moveTowardsTargets(@updateA_X2);
        updateColors_byVerticalVelocity;
      end;
      24: begin
        moveTowardsTargets(@updateA_cyclicMirror);
        updateColors_byRadius;
      end;
      PYRAMID_TARGET: fallingPyramid;
      FIREWORKS_TARGET: fireworks;
    end;
    lastModeTicks:=modeTicks;
  end;

FUNCTION TParticleEngine.capSubSteps(CONST proposedSubSteps:double; CONST otherProposedSubSteps:double=0): longint;
  CONST HARD_UPPER_BOUND=500;
        HARD_LOWER_BOUND=1;
  begin
    if      proposedSubSteps<HARD_LOWER_BOUND then result:=HARD_LOWER_BOUND
    else if (proposedSubSteps>HARD_UPPER_BOUND) or (otherProposedSubSteps>HARD_UPPER_BOUND)
    then result:=HARD_UPPER_BOUND
    else result:=ceil(max(proposedSubSteps,otherProposedSubSteps));
  end;

PROCEDURE TParticleEngine.updateA_cyclic(CONST progress: double);
  VAR i,k:longint;
      r:double;
      targetPosition:TVector3;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
     k:=(i+1) mod length(Particle);
     targetPosition:=Particle[k].p+Particle[k].v;
     r:=euklideanNorm(targetPosition);
     if      r<0.5 then targetPosition*=0.5/r
     else if r>2   then targetPosition*=2  /r;
     a:=accel(v,p,targetPosition,50,-40)-p*1E-1;
   end;
 end;

PROCEDURE TParticleEngine.updateA_cyclicMirror(CONST progress: double);
  CONST half=512;
  FUNCTION mirrored(CONST vec:TVector3):TVector3;
    begin
      result:=vec;
      result[1]:=-result[1];
    end;

  VAR i,k,modul:longint;
      targetPosition:TVector3;
  begin
    modul:=1024;
    if progress>0.2  then modul:=modul shr 1;
    if progress>0.35 then modul:=modul shr 1;
    if progress>0.5  then modul:=modul shr 1;
    if progress>0.65 then modul:=modul shr 1;
    if progress>0.8  then modul:=modul shr 1;
    modul-=1;

    for i:=0 to half-1 do with Particle[i] do begin
      k:=((i+1) and modul) or (i and not(modul));
      targetPosition:=Particle[k].p+Particle[k].v;
      a:=accel(v,p,targetPosition,50,-50);

      if sqr(p[0])+sqr(p[1])+sqr(p[2])>1 then a-=p;

      Particle[i+half].p:=mirrored(p);
      Particle[i+half].v:=mirrored(v);
      Particle[i+half].a:=mirrored(a);
    end;
  end;

PROCEDURE TParticleEngine.updateA_groupedCyclic(CONST progress: double);
  VAR i,k:longint;
      targetPosition, allPointsCenter:TVector3;
  begin
    allPointsCenter:=ZERO_VECTOR;
    for k:=0 to length(Particle)-1 do allPointsCenter+=Particle[k].p;
    allPointsCenter*=1/length(Particle);

    for i:=0 to length(Particle)-1 do begin
      k:=((i+(i shr 5)+round(progress*100)) and 31) or (i and not(31));
      with Particle[i] do begin
        targetPosition:=Particle[k].p-allPointsCenter;
        targetPosition*=1/euklideanNorm(targetPosition);
        a:=accel(v,p,targetPosition,100,-10);
      end;
   end;
 end;

PROCEDURE TParticleEngine.updateA_cube(CONST progress: double;
  CONST particleIndex: longint);
  CONST C_CubeNodes:array[0..7] of TVector3=((-1,-1,-1),(-1,-1,1),(1,-1,-1),(1,-1,1),(-1,1,-1),(-1,1,1),(1,1,-1),(1,1,1));
        C_cubeTrip1:array[0..7] of longint=(0,1,3,2,6,7,5,4);
        C_cubeTrip2:array[0..7] of longint=(0,2,6,4,5,7,3,1);
        C_cubeTrip3:array[0..7] of longint=(0,4,5,1,3,7,6,2);
  VAR k:longint;
      tau:double;
      targetPosition:TVector3;
      subCube:byte;
  begin
    with Particle[particleIndex] do begin
      subCube:=trunc(particleIndex/length(Particle)*3);
      tau:=particleIndex/length(Particle)*24+progress*2;
      k:=trunc(tau);
      tau:=frac(tau);
      case subCube of
        0: targetPosition:=(C_CubeNodes[C_cubeTrip1[ k    and 7]]*(1-tau)
                           +C_CubeNodes[C_cubeTrip1[(k+1) and 7]]*   tau)*0.8;
        1: targetPosition:=(C_CubeNodes[C_cubeTrip2[ k    and 7]]*(1-tau)
                           +C_CubeNodes[C_cubeTrip2[(k+1) and 7]]*   tau)*0.9;
      else targetPosition:=(C_CubeNodes[C_cubeTrip3[ k    and 7]]*(1-tau)
                           +C_CubeNodes[C_cubeTrip3[(k+1) and 7]]*   tau);
      end;
      a:=accel(v,p,targetPosition,20,-20);
    end;
  end;

PROCEDURE TParticleEngine.updateA_heart(CONST progress: double;
  CONST particleIndex: longint);
  VAR tau:double;
      tp :TVector3;
  begin
    with Particle[particleIndex] do begin
      tau:=(particleIndex/length(Particle)+progress)*2*pi;
      tp [2]:= (0.75  *sin(tau)-0.25  *sin(3*tau));
      tp [1]:= (0.8125*cos(tau)-0.3125*  cos(2*tau)-0.125*  cos(3*tau)-0.0625*  cos(4*tau));
      tp [0]:=0;
      a:=accel(v,p,tp+colorDelta[particleIndex]*(1-progress),50,-20)
    end;
  end;

PROCEDURE TParticleEngine.updateA_sphere(CONST progress: double;
  CONST particleIndex: longint);
  begin
    with Particle[particleIndex] do a:=accel(v,p,spherePoints[particleIndex]*progress,10,-5);
  end;

PROCEDURE TParticleEngine.updateA_swirl(CONST progress: double;
  CONST particleIndex: longint);
  FUNCTION accel(CONST v,p:TVector3):TVector3;
    VAR q:TVector3;
        f:double;
        r:double;
    begin
      q:=p;
      q[1]+=0.5;
      r:=euklideanNorm(q);
      f:=exp(-8*sqr(q[1]));
      result:=q*(-1/r);
      result[0]+=0.1*q[2]/r;
      result[2]-=0.1*q[0]/r;
      result-=v*(f*euklideanNorm(v));//*2*(exp(-8*sqr(p[2]))));
      result[1]*=2;
    end;
  CONST r0   =0.08;
  VAR angle:double;
  begin
    with Particle[particleIndex] do begin
      a:=accel(v,p);
      if (sqr(p[0])+sqr(p[1]+0.5)+sqr(p[2]))<sqr(r0) then begin
        v[1]:=5*random;
        angle:=random*2*pi;
        v[2]:=v[1]*0.1*sin(angle);
        v[0]:=v[1]*0.1*cos(angle);

        p:=v*(r0/euklideanNorm(v));
        p[1]-=0.5;
        color:=WHITE;
      end;
    end;
  end;

PROCEDURE TParticleEngine.updateA_icosahedron(CONST progress: double; CONST particleIndex: longint);
  CONST C_IcosahedronEdges:array[0..29,0..1] of byte=
    ((8,10),
     (5,7),
     (2,9),
     (1,6),
     (3,5),(5,1),(1,3),(3,0),(0,2),(2,8),(8,0),(0,4),
     (0,1),(1,2),(2,6),(6,9),(9,10),(10,11),(11,5),(5,6),(6,11),(11,7),(7,10),(10,4),(4,7),(7,3),(3,4),(4,8),(8,9),(9,11));
  VAR k:longint;
      tau,h:double;
      targetPosition,d:TVector3;
  begin
    h:=-1+3*progress;
    d:=C_IcosahedronNodes[lissajousParam[0]];
    with Particle[particleIndex] do begin
      tau:=(particleIndex/length(Particle));
      tau*=length(C_IcosahedronEdges);
      k:=trunc(tau);
      tau:=frac(tau);
      k:=k mod length(C_IcosahedronEdges);
      targetPosition:=C_IcosahedronNodes[C_IcosahedronEdges[k,0]]*(1-tau)+
                      C_IcosahedronNodes[C_IcosahedronEdges[k,1]]*(  tau);
      tau:=targetPosition*d;
      if tau>h then targetPosition-=d*(tau-h);
      a:=accel(v,p,targetPosition,30,-30);
    end;
  end;

PROCEDURE TParticleEngine.updateA_wave(CONST progress: double;
  CONST particleIndex: longint);
  VAR r:double;
      targetPosition:TVector3;
  begin
    with Particle[particleIndex] do begin
      r:=sqrt(particleIndex/1023)*2;
      targetPosition[0]:=sin(particleIndex*FIBFAK)*r;
      targetPosition[2]:=cos(particleIndex*FIBFAK)*r;
      targetPosition[1]:=-0.5+0.2*sin(3*r-progress*50);
      a:=accel(v,p,targetPosition,10,-10);
    end;
  end;

CONST GRID_SIZE=0.2;
      INV_GRID_SIZE=1/GRID_SIZE;
PROCEDURE TParticleEngine.calculateGridPositions;
  CONST OCC_RAD=20;
  VAR occupied:bitpacked array[-OCC_RAD..OCC_RAD,-OCC_RAD..OCC_RAD,-OCC_RAD..OCC_RAD] of boolean;
  PROCEDURE clearOccupied;
    VAR i,j,k:longint;
    begin
      for i:=-OCC_RAD to OCC_RAD do
      for j:=-OCC_RAD to OCC_RAD do
      for k:=-OCC_RAD to OCC_RAD do occupied[i,j,k]:=false;
    end;

  PROCEDURE markAsOccupied(CONST k:TIntVec3);
    begin
      if (k[0]>=-OCC_RAD) and (k[0]<=OCC_RAD) and
         (k[1]>=-OCC_RAD) and (k[1]<=OCC_RAD) and
         (k[2]>=-OCC_RAD) and (k[2]<=OCC_RAD) then occupied[k[0],k[1],k[2]]:=true;
    end;

  FUNCTION isOccupied(CONST k:TIntVec3):boolean;
    begin
      result:=(k[0]>=-OCC_RAD) and (k[0]<=OCC_RAD) and
              (k[1]>=-OCC_RAD) and (k[1]<=OCC_RAD) and
              (k[2]>=-OCC_RAD) and (k[2]<=OCC_RAD) and
              occupied[k[0],k[1],k[2]];
    end;

  FUNCTION findVacantSpot(CONST k:TIntVec3):TIntVec3;
    VAR radius:longint=0;
        di,dj,dk:longint;
    begin
      while true do begin
        for di:=-radius to radius do
        for dj:=-radius to radius do
        for dk:=-radius to radius do
        if max(max(abs(di),abs(dj)),abs(dk))=radius then begin
          result:=k;
          result[0]+=di;
          result[1]+=dj;
          result[2]+=dk;
          if not(isOccupied(result)) then exit(result);
        end;
        inc(radius);
      end;
    end;

  VAR i:longint;
  begin
    clearOccupied;
    for i:=0 to length(Particle)-1 do begin
      gridPoint[i]:=findVacantSpot(roundVector(Particle[i].p*INV_GRID_SIZE));
      markAsOccupied(gridPoint[i]);
    end;
  end;

PROCEDURE TParticleEngine.updateA_grid(CONST progress: double;
  CONST particleIndex: longint);
  begin
    with Particle[particleIndex] do a:=accel(v,p,gridPoint[particleIndex]*GRID_SIZE,5,-20);
  end;

PROCEDURE TParticleEngine.updateA_lissajous(CONST progress: double;
  CONST particleIndex: longint);
  VAR t,tau:double;
      targetPosition:TVector3;
  begin
     t:=sqr(progress);
     with Particle[particleIndex] do begin
       tau:=(particleIndex/length(Particle)-t)*2*pi;
       targetPosition[0]:=sin(     lissajousParam[0]*tau);
       targetPosition[1]:=sin(pi/6+lissajousParam[1]*tau);
       targetPosition[2]:=sin(pi/3+lissajousParam[2]*tau);
       a:=accel(v,p,targetPosition,20,-2);
     end;
  end;

PROCEDURE TParticleEngine.updateA_vogler(CONST progress: double;
  CONST particleIndex: longint);
  VAR k:longint;
      targetPosition:TVector3;

      refY:double;
      downcurve:double;
      radius:double;
  begin
    k:=trunc(progress*20);
    downcurve:=0.4*k/20;
    radius   :=1/30*k/20;

    k:=((k div 2)*21+13*(k and 1));
    refY:=k*0.01-1;
    with Particle[particleIndex] do begin
      if particleIndex-k>0 then begin
        targetPosition[0]:=sin((particleIndex-k)*FIBFAK)*sqrt((particleIndex-k))*radius;
        targetPosition[2]:=cos((particleIndex-k)*FIBFAK)*sqrt((particleIndex-k))*radius;
        targetPosition[1]:=refY-downcurve*sqr((particleIndex-k)/1023);
      end else begin
        targetPosition[0]:=0;
        targetPosition[2]:=0;
        if odd(particleIndex)
        then targetPosition[1]:=refY+(particleIndex-k)*0.01
        else begin
          targetPosition[1]:=sin((1+refY+(particleIndex-k)*0.01)*0.5)-1+k*0.003;
          targetPosition[2]:=cos((1+refY+(particleIndex-k)*0.01)*0.5)-1;
        end;
      end;
      a:=accel(v,p,targetPosition,10,-10);
    end;
  end;

PROCEDURE TParticleEngine.updateCol_vogler(CONST progress, dt: double);
  CONST GREEN:TVector3=(0,0.5,0);
        ORANGE:TVector3=(1,0.5,0);
        WHITE:TVector3=(0.8,0.9,1);
  VAR i,k:longint;
  begin
    k:=trunc(progress*20);
    k:=((k div 2)*21+13*(k and 1));
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      if i-k>0 then begin
        if i-k>100 then color+=(WHITE-color)*(0.1*dt)
                   else color+=(ORANGE-color)*(0.2*dt);
      end else
       color+=(GREEN-color)*(dt);
    end;
  end;

PROCEDURE TParticleEngine.updateA_CIRCL(CONST progress: double);
  CONST spring=5;
        ATTENUATION=-2;
  VAR i,k:longint;
      tau:double;
      targetPosition:TVector3;
  begin
    k:=0;
    while k<length(Particle) do begin
      tau:=(k/length(Particle)+progress)*2*pi;
      targetPosition[0]:=sin(     lissajousParam[0]*tau);
      targetPosition[1]:=sin(pi/6+lissajousParam[1]*tau);
      targetPosition[2]:=sin(pi/3+lissajousParam[2]*tau);
      with Particle[k] do a:=accel(v,p,targetPosition,spring,ATTENUATION);
      for i:=k+1 to k+31 do with Particle[i] do begin
        targetPosition:=Particle[i-1].p *progress+targetPosition*(1-progress);
        a:=accel(v,p,targetPosition,spring,ATTENUATION)
      end;
      k+=32
    end;
  end;

CONST C_clusterChunk:array[0..12] of longint=(0,85,171,256,341,427,512,597,683,768,853,939,1024);
PROCEDURE TParticleEngine.updateA_clusters(CONST progress: double);
  VAR i,k:longint;
      tgt:TVector3;
      r,spring:double;
  begin
    for k:=0 to 11 do begin
      tgt:=C_IcosahedronNodes[k];
      spring:=5;
      with Particle[C_clusterChunk[k]] do begin
        a:=tgt-p;
        r:=euklideanNorm(a);
        a:=a*(spring/r)-v*5;
        tgt:=p*0.4+tgt*0.6;
      end;
      for i:=C_clusterChunk[k]+1 to C_clusterChunk[k+1]-1 do with Particle[i] do begin
        spring*=0.999;
        a:=tgt-p;
        r:=euklideanNorm(a);
        a:=a*(spring/r)-v*5;
        tgt:=p*0.4+tgt*0.6;
      end;
    end;
  end;

PROCEDURE TParticleEngine.updateA_sheet(CONST progress: double);
  VAR ix,iy,k,n:longint;
      allPointsCenter, targetPosition:TVector3;
  begin
    allPointsCenter:=ZERO_VECTOR;
    for k:=0 to length(Particle)-1 do allPointsCenter+=Particle[k].p;
    allPointsCenter*=1/length(Particle);

    for ix:=0 to 31 do
    for iy:=0 to 31 do with Particle[ix shl 5 or iy] do begin
      targetPosition:=ZERO_VECTOR; n:=0;
      if ix> 0 then begin targetPosition+=Particle[(ix-1) shl 5 or iy].p; n+=1; end;
      if ix<31 then begin targetPosition+=Particle[(ix+1) shl 5 or iy].p; n+=1; end;
      if iy> 0 then begin targetPosition+=Particle[ix shl 5 or (iy-1)].p; n+=1; end;
      if iy<31 then begin targetPosition+=Particle[ix shl 5 or (iy+1)].p; n+=1; end;
      targetPosition:=targetPosition*(1/n);
      a:=accel(v,p,targetPosition,40,-2);
      if n<4 then begin
        targetPosition:=p-allPointsCenter;
        a+=targetPosition*((2-1.5*progress)/(sqr(targetPosition[0])+sqr(targetPosition[1])+sqr(targetPosition[2])));
        a-=allPointsCenter;
      end;
    end;
  end;

PROCEDURE TParticleEngine.updateColors_sheet(CONST progress, dt: double);
  VAR targetColor:TVector3;
      ix,iy:longint;
      pk,k:longint;
  begin
    pk:=trunc(progress*200) and 15;
    for ix:=0 to 31 do
    for iy:=0 to 31 do with Particle[ix shl 5 or iy] do begin
      k:=(44-2*round(sqrt(sqr(ix-15.5)+sqr(iy-15.5)))) and 15;
      if k=pk
      then color:=WHITE-commonTargetColor
      else begin
        targetColor:=commonTargetColor;
        color+=(targetColor-color)*(2*dt);
      end;
    end;
  end;

PROCEDURE TParticleEngine.updateA_bicyclic(CONST progress: double);
  VAR ix,iy,k:longint;
      allPointsCenter, targetPosition:TVector3;
      tgt1,tgt2:TVector3;
      r1,r2:double;
  begin
    allPointsCenter:=ZERO_VECTOR;
    for k:=0 to length(Particle)-1 do allPointsCenter+=Particle[k].p;
    allPointsCenter*=1/length(Particle);

    for ix:=0 to 31 do
    for iy:=0 to 31 do with Particle[ix shl 5 or iy] do begin
      k:=((ix+ 1) and 31) shl 5 or iy;
      tgt1:=Particle[k].p+Particle[k].v-allPointsCenter;
      r1:=euklideanNorm(tgt1-p);
      k:=ix shl 5 or ((iy+ 1) and 31);
      tgt2:=Particle[k].p+Particle[k].v-allPointsCenter;
      r2:=euklideanNorm(tgt2-p);

      if r1>r2 then targetPosition:=tgt1 else targetPosition:=tgt2;
      a:=accel(v,p,targetPosition,10,-10);
    end;
  end;

PROCEDURE TParticleEngine.updateA_sliver(CONST progress: double);
  VAR i:longint;
      tgt:TVector3=(0,0.5,0);
  begin
    tgt[1]:=-1+1.5*progress;
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      if i/length(Particle)<progress*2
      then begin
        a:=(tgt-p);
        a*=5/(0.1+euklideanNorm(a));
      end
      else a:=ZERO_VECTOR;
      a[1]-=1;
      if p[1]<=-1 then begin
        p[1]:=-1;
        v[1]:=0.9*abs(v[1]);
        color:=WHITE;
      end;
      tgt:=tgt*0.99+p*0.01;
      a-=v*0.8;
    end;
  end;

PROCEDURE TParticleEngine.updateA_byDistance(CONST progress: double);
  VAR i:longint;
      d:TVector3;
      targetRadius,currentRadius:double;
  begin
    with Particle[0] do begin
      a:=accel(v,p,ZERO_VECTOR,14,-20);
      d:=p+v;
    end;
    for i:=1 to length(Particle)-1 do with Particle[i] do begin
      targetRadius:=2*sqrt(i/length(Particle));
      currentRadius:=euklideanNorm(p);
      a:=accel(v,p,d*(targetRadius/euklideanNorm(d)),14,-20);
      if currentRadius<targetRadius then a+=p*(1/currentRadius*(targetRadius-currentRadius)*10);
      d:=p+v;
    end;
  end;

PROCEDURE TParticleEngine.lorenzAttractor(CONST dt: double);
  FUNCTION dL(CONST x:TVector3):TVector3;
    CONST spatial_scaling=20;
          speed_scaling=0.02;
          yAxisShift=1;
    begin
      result[0]:=speed_scaling*(10*(x[2]-x[0])                                       );
      result[2]:=speed_scaling*(28* x[0]-x[2]-x[0]*(x[1]+yAxisShift) *spatial_scaling);
      result[1]:=speed_scaling*(    x[0]*x[2]*spatial_scaling-8/3*(x[1]+yAxisShift)  );
      result*=10;
    end;
  CONST maxTimeStep=0.001;

  VAR i,k,subSteps:longint;
      dtSub:double;
      dx0,dx1,dx2,dx3:TVector3;
  begin
    subSteps:=capSubSteps(dt/maxTimeStep);
    dtSub:=dt/subSteps;
    for i:=0 to length(Particle)-1 do with Particle[i] do
    for k:=1 to subSteps do begin
      dx0:=dL(p        )*dtSub;
      dx1:=dL(p+dx0*0.5)*dtSub;
      dx2:=dL(p+dx1*0.5)*dtSub;
      dx3:=dL(p+dx2    )*dtSub;
      v:=dx0*(1/6)+dx1*(1/3)+dx2*(1/3)+dx3*(1/6);
      p+=v;
      v*=1/dtSub;
      if p[0]<0 then color+=(      commonTargetColor-color)*(0.4*dtSub)
                else color+=(WHITE-commonTargetColor-color)*(0.4*dtSub);
    end;
  end;

PROCEDURE TParticleEngine.thomasAttractor(CONST dt: double);
  FUNCTION dL(CONST x:TVector3):TVector3;
    CONST spatial_scaling=4;
          b=0.18;
    begin
      result[0]:=sin(x[1]*spatial_scaling)-b*spatial_scaling*x[0];
      result[1]:=sin(x[2]*spatial_scaling)-b*spatial_scaling*x[1];
      result[2]:=sin(x[0]*spatial_scaling)-b*spatial_scaling*x[2];
    end;
  CONST maxTimeStep=0.01;

  VAR i,k,subSteps:longint;
      dtSub:double;
      dx0,dx1,dx2,dx3:TVector3;
  begin
    subSteps:=capSubSteps(dt/maxTimeStep);
    dtSub:=dt/subSteps;
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      for k:=1 to subSteps do begin
        dx0:=dL(p        )*dtSub;
        dx1:=dL(p+dx0*0.5)*dtSub;
        dx2:=dL(p+dx1*0.5)*dtSub;
        dx3:=dL(p+dx2    )*dtSub;
        v:=dx0*(1/6)+dx1*(1/3)+dx2*(1/3)+dx3*(1/6);
        p+=v;
        if p[0]+p[1]+p[2]<0
        then color+=(      commonTargetColor-color)*(0.4*dtSub)
        else color+=(WHITE-commonTargetColor-color)*(0.4*dtSub);
      end;
      v*=1/dtSub;
    end;
  end;

PROCEDURE TParticleEngine.updateA_X1(CONST progress: double);
  VAR i:longint;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      a[0]:= p[1];
      a[1]:=-p[0];
      a[2]:=5*(sqr(p[1])-sqr(p[0]));
      a*=2/vectors.sumOfSquares(a);
      a-=p*0.6;
      a-=v*0.7;
      a*=2;
    end;
  end;

PROCEDURE TParticleEngine.updateA_X2(CONST progress: double);
  VAR i:longint;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      a[0]:= p[1];
      a[1]:=-p[0];
      a[2]:=0;
      a*=cos(p[0]*10)/(sqr(a[0])+sqr(a[1]));
      a[2]:=sin(p[1]);
      a-=p;
      a-=v*(1-sqr(2*progress-1));
      a*=5;
    end;
  end;

PROCEDURE TParticleEngine.updateA_clock(CONST progress: double;
  CONST particleIndex: longint);
  CONST BAR_POS:array[0..6,0..1] of TVector3=
           (((-0.2 , 0.5 ,0),( 0.2 , 0.5 ,0)),
            ((-0.25, 0.05,0),(-0.25, 0.45,0)),
            (( 0.25, 0.05,0),( 0.25, 0.45,0)),
            ((-0.2 , 0   ,0),( 0.2 , 0   ,0)),
            ((-0.25,-0.05,0),(-0.25,-0.45,0)),
            (( 0.25,-0.05,0),( 0.25,-0.45,0)),
            ((-0.2 ,-0.5 ,0),( 0.2 ,-0.5 ,0)));
        DIGIT_SHIFT:array[0..5] of TVector3=((-1.6,0,0),(-1.0,0,0),(-0.3,0,0),(0.3,0,0),(1.0,0,0),(1.6,0,0));
        POINT_POS:array[0..3] of TVector3=((-0.65,-0.1,0),(-0.65,0.1,0),(0.65,-0.1,0),(0.65,0.1,0));

  VAR digit,bar:longint;
      q:double;
      targetPosition:TVector3;
  begin
    with Particle[particleIndex] do begin
      if particleIndex<1008 then begin
        digit:= particleIndex div 24;
        bar  := digit mod 7; digit:=digit div 7;
        q    := (particleIndex mod 24)/23*24/23;
        targetPosition:=DIGIT_SHIFT[digit]+ BAR_POS[bar,0]*q+BAR_POS[bar,1]*(1-q);
      end else begin
        q:=(particleIndex-1008)/(1024-1008)*2*pi;
        targetPosition:=POINT_POS[particleIndex and 3];
        targetPosition[0]+=0.01*sin(q+progress);
        targetPosition[1]+=0.01*cos(q+progress);
      end;
      a:=accel(v,p,targetPosition,50,-20);
    end;
  end;

PROCEDURE TParticleEngine.updateColors_clock(CONST dt: double);
  CONST
    BAR_ACTIVE:array['0'..'9',0..6] of boolean=((true,true,true,false,true,true,true),
                                                (false,false,true,false,false,true,false),
                                                (true,false,true,true,true,false,true),
                                                (true,false,true,true,false,true,true),
                                                (false,true,true,true,false,true,false),
                                                (true,true,false,true,false,true,true),
                                                (true,true,false,true,true,true,true),
                                                (true,false,true,false,false,true,false),
                                                (true,true,true,true,true,true,true),
                                                (true,true,true,true,false,true,true));

  VAR i,digit,bar:longint;
      timeString:string;
  begin
    timeString:=FormatDateTime('hhmmss',now);
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      if i<1008 then begin
        digit:= i div 24;
        bar  := digit mod 7; digit:=digit div 7;
        if BAR_ACTIVE[timeString[digit+1],bar]
        then color+=(commonTargetColor    -color)*((digit+1)*dt)
        else color+=(commonTargetColor*0.2-color)*((digit+1)*dt);
      end else begin
        color+=(commonTargetColor    -color)*(dt)
      end;
    end;
  end;

CONSTRUCTOR TParticleEngine.create;
  VAR i,j: integer;
      acceptAt:double=2;
      accept:boolean;
      temp:TVector3;
  begin
    MODE_SWITCH_INTERVAL_IN_TICKS:=20000;
    TICKS_PER_SIMULATION_TIME_UNIT:=1000;
    lockCurrentSetup:=false;

    spherePoints[0]:=randomOnSphere;
    for i:=1 to length(spherePoints)-1 do begin
      repeat
        spherePoints[i]:=randomOnSphere;
        accept:=true;
        for j:=0 to i-1 do accept:=accept and (euklideanNorm(spherePoints[i]-spherePoints[j])>acceptAt);
        if not(accept) then acceptAt*=0.9999;
      until accept;

      j:=i;
      while (j>0) and (spherePoints[j,1]>spherePoints[j-1,1]) do begin
        temp:=spherePoints[j-1];
        spherePoints[j-1]:=spherePoints[j];
        spherePoints[j]:=temp;
        dec(j);
      end;
    end;

    for i:=0 to length(colorDelta)-1 do colorDelta[i]:=randomInSphere*0.08;

    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      p:=randomOnSphere;
      v:=randomOnSphere;
      color[0]:=random;
      color[1]:=random;
      color[2]:=random;
    end;

    attractionMode:=ATTRACTION_MODE_COUNT;
    switchAttractionMode;
    lastModeTicks:=0;
  end;

DESTRUCTOR TParticleEngine.destroy;
  begin
    inherited destroy;
  end;

FUNCTION TParticleEngine.update(VAR modeTicks: longint): single;
  begin
    if modeTicks<=lastModeTicks then modeTicks:=lastModeTicks+1;
    result:=(modeTicks-lastModeTicks)*1E-3; //approximate dt in seconds
    MoveParticles(modeTicks);
    if (modeTicks>MODE_SWITCH_INTERVAL_IN_TICKS) and not(lockCurrentSetup) then begin
      switchAttractionMode;
      modeTicks    :=0;
      lastModeTicks:=0;
    end;
  end;

PROCEDURE TParticleEngine.nextSetup(VAR modeTicks: longint; CONST forcedMode:byte=255);
  begin
    modeTicks:=0;
    lastModeTicks:=0;
    switchAttractionMode(forcedMode);
  end;

PROCEDURE TParticleEngine.DrawParticles(CONST ParticleList: GLuint; CONST particleRotX,particleRotY:GLfloat);
  VAR i: integer;
  begin
    for i:=0 to length(Particle)-1 do begin
      glColor3f(Particle[i].color[0],Particle[i].color[1],Particle[i].color[2]);
      glPushMatrix;
      glTranslatef(Particle[i].p[0], Particle[i].p[1], Particle[i].p[2]);
      glRotatef(particleRotY,0.0,1.0,0.0);
      glRotatef(particleRotX,0.1,0.0,0.0);

      glCallList(ParticleList);
      glPopMatrix;
    end;
  end;

FUNCTION TParticleEngine.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR readAttractionMode:byte;
  begin
    lockCurrentSetup:=stream.readBoolean;
    MODE_SWITCH_INTERVAL_IN_TICKS:=stream.readNaturalNumber;
    TICKS_PER_SIMULATION_TIME_UNIT:=stream.readDouble;
    readAttractionMode:=stream.readByte;
    if lockCurrentSetup then switchAttractionMode(readAttractionMode);
    result:=stream.allOkay;
  end;

PROCEDURE TParticleEngine.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    stream.writeBoolean(lockCurrentSetup);
    stream.writeNaturalNumber(MODE_SWITCH_INTERVAL_IN_TICKS);
    stream.writeDouble(TICKS_PER_SIMULATION_TIME_UNIT);
    stream.writeByte(attractionMode);
  end;

PROCEDURE TParticleEngine.switchAttractionMode(CONST forcedMode: byte);
  PROCEDURE prepareForPyramid;
    VAR i:longint;
    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        p[1]+=1;
        v:=p*(5/vectors.sumOfSquares(p));
        p[1]-=1;
      end;
    end;

  CONST primes:array[0..2] of byte=(2,3,5);
  VAR m,p:byte;

      tmp:TParticle;
      i,k:longint;

  begin
    if forcedMode<ATTRACTION_MODE_COUNT
    then m:=forcedMode
    else repeat m:=random(ATTRACTION_MODE_COUNT) until m<>attractionMode;

    lissajousParam[0]:=1+random(16);
    lissajousParam[1]:=1+random(16);
    lissajousParam[2]:=1+random(16);

    repeat commonTargetColor:=vectorOf(random,random,random); until commonTargetColor[0]+commonTargetColor[1]+commonTargetColor[2]>1;
    commonSaturation:=0.3+0.7*random;
    commonHueOffset:=random;

    for p in primes do
    while (lissajousParam[0] mod p=0) and
          (lissajousParam[1] mod p=0) and
          (lissajousParam[2] mod p=0) do begin
      lissajousParam[0]:=lissajousParam[0] div p;
      lissajousParam[1]:=lissajousParam[1] div p;
      lissajousParam[2]:=lissajousParam[2] div p;
    end;

    for k:=0 to length(Particle)-1 do begin
      i:=random(length(Particle));

      tmp        :=Particle[i];
      Particle[i]:=Particle[k];
      Particle[k]:=tmp;
    end;
    attractionMode:=m;
    if attractionMode=GRID_TARGET then calculateGridPositions;
    if attractionMode=PYRAMID_TARGET then prepareForPyramid;
    if attractionMode=ICOSAHEDRON_TARGET then lissajousParam[0]:=random(length(C_IcosahedronNodes));
    if attractionMode=FIREWORKS_TARGET then commonSaturation:=-10;
  end;

end.

