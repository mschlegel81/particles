UNIT particlePhysics;

{$mode objfpc}{$H+}

INTERFACE
USES vectors,gl;

TYPE
  FUpdateAcceleration=PROCEDURE(CONST progress:double) of object;

  TParticle = record
    p,v,a,
    color:TVector3;
  end;

  { TParticleEngine }

  TParticleEngine = class
  private
    //Pseudo constants
    spherePoints: array [0..1023] of TVector3;
    //State
    attractionMode:byte;
    commonTargetColor:TVector3;
    lissajousParam:array[0..2] of byte;
    Particle: array [0..1023] of TParticle;

    PROCEDURE updateA_cyclic(CONST progress:double);
    PROCEDURE updateA_groupedCyclic(CONST progress:double);
    PROCEDURE updateA_cube(CONST progress:double);
    PROCEDURE updateA_heart(CONST progress:double);
    PROCEDURE updateA_sphere(CONST progress:double);
    PROCEDURE updateA_swirl(CONST progress:double);
    PROCEDURE updateA_icosahedron(CONST progress:double);
    PROCEDURE updateA_wave(CONST progress:double);
    PROCEDURE updateA_grid(CONST progress:double);
    PROCEDURE updateA_lissajous(CONST progress:double);
    PROCEDURE updateA_vogler(CONST progress:double);
    PROCEDURE updateCol_vogler(CONST progress,dt: double);
    PROCEDURE updateA_CIRCL(CONST progress:double);
    PROCEDURE updateA_clusters(CONST progress:double);
    PROCEDURE updateCol_clusters(CONST dt:double);
    PROCEDURE updateA_sheet(CONST progress:double);
    PROCEDURE updateA_bicyclic(CONST progress:double);
    PROCEDURE updateA_clock(CONST progress:double);

  public
    CONSTRUCTOR create;
    DESTRUCTOR destroy; override;
    PROCEDURE switchAttractionMode;
    PROCEDURE MoveParticles(CONST modeTicks:longint; CONST dt:double);
    PROCEDURE DrawParticles(CONST ParticleList: GLuint);
    PROPERTY currentAttractionMode:byte read attractionMode;
  end;

CONST
  CLOCK_TARGET=20;
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

IMPLEMENTATION
USES math,sysutils,LCLProc;
CONST
  WHITE:TVector3=(1,1,1);
  ATTRACTION_MODE_COUNT=21;
  FIBFAK=2*pi/sqr((sqrt(5)-1)/2);

PROCEDURE TParticleEngine.MoveParticles(CONST modeTicks: longint; CONST dt: double);

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
      end;
    end;

  PROCEDURE special_lorenzAttractor;
    FUNCTION dL(CONST x:TVector3):TVector3;
      CONST spatial_scaling=20;
            speed_scaling=0.02;
            yAxisShift=1;
      begin
        result[0]:=speed_scaling*(10*(x[2]-x[0])                                       );
        result[2]:=speed_scaling*(28* x[0]-x[2]-x[0]*(x[1]+yAxisShift) *spatial_scaling);
        result[1]:=speed_scaling*(    x[0]*x[2]*spatial_scaling-8/3*(x[1]+yAxisShift)  );
      end;

    VAR i:longint;
        dx0,dx1,dx2,dx3:TVector3;

    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        dx0:=dL(p        )*dt;
        dx1:=dL(p+dx0*0.5)*dt;
        dx2:=dL(p+dx1*0.5)*dt;
        dx3:=dL(p+dx2    )*dt;
        v:=dx0*(1/6)+dx1*(1/3)+dx2*(1/3)+dx3*(1/6);
        p+=v*10;
        v*=1/dt;
        if p[0]<0 then color+=(      commonTargetColor-color)*(0.4*dt)
                  else color+=(WHITE-commonTargetColor-color)*(0.4*dt);
      end;
    end;

  //PROCEDURE updateTargets_byDistance;
  //  VAR i,k:longint;
  //      d:TVector3;
  //  begin
  //    Particle[0].targetPosition:=ZERO_VECTOR;
  //    for i:=1 to 1023 do with Particle[i] do begin
  //      k:=(i-1) shr 1;
  //      d:=p-Particle[k].p;
  //      d*=0.2/euklideanNorm(d);
  //      targetPosition:=Particle[k].targetPosition+d;
  //    end;
  //  end;

  PROCEDURE fallAndBounce(CONST i:longint);
    CONST BLUE  :TVector3=(0,0,1);
    VAR acc:TVector3;
        hitTime:double;
    begin
      with Particle[i] do begin
        acc[0]:=0;
        acc[1]:=-1;
        acc[2]:=0;
        v+=(acc-v*(euklideanNorm(v)*0.5))*dt;
        if p[1]<=-1 then begin
          p[1]:=-1;
          v[1]:=1;
          color:=BLUE;
        end;
        if (v[1]<0) and (p[1]+v[1]*dt<=-1) then begin
          hitTime:=(-1-p[1])/v[1];
          v[1]:=0.9*abs(v[1]);
          p[1]:=-1+v[1]*(dt-hitTime);
          color:=color*0.7+BLUE*0.3;
        end else p+=v*dt;
      end;
    end;

  PROCEDURE moveTowardsTargets(CONST updateA:FUpdateAcceleration; CONST iMax_:longint=1023);
    //
    //FUNCTION dampn(CONST v:TVector3):TVector3;
    //  begin result:=v*(euklideanNorm(v)*dampingFactor); end;

    VAR i,imax:longint;
        aMax:double=0;
        tmp,dtSub:double;
        subSteps,k:longint;
    begin
      if iMax_>=length(Particle)
      then imax:=length(Particle)-1
      else imax:=iMax_;

      updateA(modeTicks/20000);
      for i:=0 to imax do with Particle[i] do begin
        tmp:=sqr(a[0])+sqr(a[1])+sqr(a[2]);
        if (tmp>aMax) and not(isInfinite(tmp)) and not(isNan(tmp)) then aMax:=tmp;
      end;
      aMax:=sqrt(aMax);
      subSteps:=trunc(aMax*dt*10);
      if subSteps<=0 then subSteps:=1 else if subSteps>1000 then subSteps:=1000;
      dtSub:=dt/subSteps;

      DebugLn('substeps: '+intToStr(subSteps));

      for k:=1 to subSteps do begin
        if k>1 then updateA(modeTicks/20000+dtSub*(k-1)*0.05);
        for i:=0 to imax do with Particle[i] do begin
          v+=a*dtSub;
          p+=v*dtSub;
        end;
      end;
      for i:=imax+1 to length(Particle)-1 do with Particle[i] do fallAndBounce(i);
    end;

  PROCEDURE special_sliver;
    VAR i:longint;
        acc:TVector3;
        tgt:TVector3=(0,0.5,0);
    begin

      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        if i/length(Particle)<modeTicks/10000
        then begin
          acc:=(tgt-p);
          acc*=0.5/(0.1+euklideanNorm(acc));
        end
        else acc:=ZERO_VECTOR;
        acc[1]-=0.1;
        if p[1]<=-1 then begin
          p[1]:=-1;
          v[1]:=0.9*abs(v[1]);
        end;
        tgt:=tgt*0.99+p*0.01;

        v+=(acc-v*(euklideanNorm(v)*5))*dt;
        p+=v*(10*dt);
      end;
    end;

  PROCEDURE updateColors_rainbow;
    VAR tgt:TVector3;
        i:longint;
    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do begin
        tgt:=hsvColor(i/length(Particle)+commonTargetColor[0],commonTargetColor[1],1);
        color+=(tgt-color)*(0.1*dt);
      end;
    end;

  PROCEDURE updateColors_commmonTarget;
    VAR i:longint;

    begin
      for i:=0 to length(Particle)-1 do with Particle[i] do
        color+=(commonTargetColor+spherePoints[(i*31) and 1023]*0.1-color)*(dt);
    end;

  PROCEDURE updateColors_reds;
    begin
      commonTargetColor[2]-=commonTargetColor[2]*(0.3*dt);
      commonTargetColor[1]-=commonTargetColor[1]*(0.2*dt);
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
        tgt:=hsvColor(euklideanNorm(p)+commonTargetColor[0],1,1);
        color+=(tgt-color)*(dt);
      end;
    end;

  VAR i: integer;
  begin
    fixBrokenPositions;
    attractionMode:=11;
    case attractionMode of
      0: begin moveTowardsTargets(@updateA_cyclic);                                         updateColors_rainbow;            end;
      1: begin moveTowardsTargets(@updateA_cube  ,round(length(Particle)*modeTicks/10000)); updateColors_commmonTarget;      end;
      2: begin moveTowardsTargets(@updateA_sphere,round(length(Particle)*modeTicks/10000)); updateColors_commmonTarget;      end;
      3: begin moveTowardsTargets(@updateA_heart);                                          updateColors_reds;               end;
      4: begin moveTowardsTargets(@updateA_wave);                                           updateColors_byVerticalVelocity; end;
      5: begin moveTowardsTargets(@updateA_swirl);                                          updateColors_byRadius;           end;
      6: begin moveTowardsTargets(@updateA_grid);                                           updateColors_commmonTarget;      end;
      7: begin
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
      8: begin moveTowardsTargets(@updateA_lissajous);      updateColors_rainbow;  end;
      9: begin moveTowardsTargets(@updateA_clusters); updateCol_clusters(dt);  end;
     10: begin moveTowardsTargets(@updateA_vogler); updateCol_vogler(modeTicks/20000, dt);  end;
     11: begin moveTowardsTargets(@updateA_CIRCL);  updateColors_rainbow;           end;
    end;

   // case attractionMode of
   //   //Cyclic attraction
   //  12: begin special_sliver; updateColors_byRadius; end;
   //  13: begin updateTargets_sheet(modeTicks/20000);         moveTowardsTargets(-100, 5); end;
   //  14: begin
   //        for i:=0 to length(Particle)-1 do with Particle[i] do begin
   //          fallAndBounce(i);
   //          if (sqr(p[0])+sqr(p[1]+1)+sqr(p[2])<1E-2) then begin
   //            v[1]+=0.5;
   //            v+=randomOnSphere*0.02;
   //            color:=vectorOf(1,1,random);
   //            color[1]:=0.5+0.5*color[2];
   //
   //          end else if p[1]<=-0.99 then begin
   //             v[0]-=0.1*dt*p[0];
   //             v[2]-=0.1*dt*p[2];
   //          end;
   //        end;
   //      end;
   //  15: begin updateTargets_icosahedron(modeTicks/20000);   moveTowardsTargets(-200,6,round(length(Particle)*modeTicks/10000)); updateColors_byVerticalVelocity; end;
   //  16: begin updateTargets_groupedCyclic(modeTicks/20000); moveTowardsTargets(-120,5);  updateColors_commmonTarget; end;
   //  17: begin special_lorenzAttractor;                                                              end;
   //  18: begin updateTargets_bicyclic(modeTicks/20000);      moveTowardsTargets(-100, 5); updateColors_byVerticalVelocity;                             end;
   //  19: begin updateTargets_byDistance;    moveTowardsTargets(-10, 1); updateColors_byRadius;                            end;
   //  20: begin updateTargets_clock(modeTicks/20000); moveTowardsTargets(-200, 5); end;
   // end;
  end;

FUNCTION accel(CONST v,p,target:TVector3; CONST springConstant,dampingFctor:double):TVector3;
  begin result:=(target-p)*springConstant+v*(euklideanNorm(v)*dampingFctor); end;

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
     a:=accel(v,p,targetPosition,50,-50);
   end;
 end;

PROCEDURE TParticleEngine.updateA_groupedCyclic(CONST progress: double);
  VAR i,k:longint;
      targetPosition:TVector3;
  begin
    for i:=0 to length(Particle)-1 do begin
     k:=((i+(i shr 5)+round(progress*100)) and 31) or (i and not(31));
     with Particle[i] do begin
       targetPosition:=Particle[k].p+Particle[k].v;
       targetPosition*=1/euklideanNorm(targetPosition);
       a:=accel(v,p,targetPosition,1,-100);
     end;
   end;
 end;

PROCEDURE TParticleEngine.updateA_cube(CONST progress: double);
  CONST C_cubeNodes:array[0..7] of TVector3=((-1,-1,-1),(-1,-1,1),(1,-1,-1),(1,-1,1),(-1,1,-1),(-1,1,1),(1,1,-1),(1,1,1));
        C_cubeTrip1:array[0..7] of longint=(0,1,3,2,6,7,5,4);
        C_cubeTrip2:array[0..7] of longint=(0,2,6,4,5,7,3,1);
        C_cubeTrip3:array[0..7] of longint=(0,4,5,1,3,7,6,2);
  VAR i,k:longint;
      tau:double;
      targetPosition:TVector3;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      tau:=i/length(Particle)*8+progress*2;
      k:=trunc(tau);
      tau:=frac(tau);
      case i mod 3 of
        0: targetPosition:=C_cubeNodes[C_cubeTrip1[ k    and 7]]*(1-tau)
                          +C_cubeNodes[C_cubeTrip1[(k+1) and 7]]*   tau;
        1: targetPosition:=(C_cubeNodes[C_cubeTrip2[ k    and 7]]*(1-tau)
                           +C_cubeNodes[C_cubeTrip2[(k+1) and 7]]*   tau)*0.98;
      else targetPosition:=(C_cubeNodes[C_cubeTrip3[ k    and 7]]*(1-tau)
                           +C_cubeNodes[C_cubeTrip3[(k+1) and 7]]*   tau)*0.96;
      end;
      a:=accel(v,p,targetPosition,10,-10);
    end;
  end;

PROCEDURE TParticleEngine.updateA_heart(CONST progress: double);
  VAR i,k:longint;
      tau:double;
      targetPosition:TVector3;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      tau:=(i/length(Particle)+progress)*2*pi;
      k:=i and 3;
      targetPosition[2]:=sin(k*progress*pi)*(1-0.1*k)*(0.75  *sin(tau)-0.25  *sin(3*tau));
      targetPosition[0]:=cos(k*progress*pi)*(1-0.1*k)*(0.75  *sin(tau)-0.25  *sin(3*tau));
      targetPosition[1]:=(1-0.1*k)*(0.8125*cos(tau)-0.3125*cos(2*tau)-0.125*cos(3*tau)-0.0625*cos(4*tau));
      a:=accel(v,p,targetPosition,10,-20);
    end;
  end;

PROCEDURE TParticleEngine.updateA_sphere(CONST progress: double);
  VAR i:longint;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do
      a:=accel(v,p,spherePoints[i]*progress,10,-5);
  end;

PROCEDURE TParticleEngine.updateA_swirl(CONST progress: double);
  FUNCTION accel(CONST v,p:TVector3; CONST x0:double):TVector3;
    VAR f:double;
    begin
      f:=exp(-4*sqr(p[2]));
      result[0]:=-p[0]+f*p[1];
      result[1]:=-p[1]-f*p[0];
      result[2]:=0.1*(x0-p[2]);
      result-=v*(euklideanNorm(v));
    end;

  VAR i:longint;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do
      a:=accel(v,p,0.5-i/length(Particle));
  end;

PROCEDURE TParticleEngine.updateA_icosahedron(CONST progress: double);
  CONST C_IcosahedronEdges:array[0..29,0..1] of byte=
    ((0,1),(1,2),(2,6),(6,9),(9,10),(10,11),(11,5),(5,6),(6,11),(11,7),(7,10),(10,4),(4,7),(7,3),(3,4),(4,8),(8,9),(9,11),
     (3,5),(5,1),(1,3),(3,0),(0,2),(2,8),(8,0),(0,4),
     (8,10),
     (5,7),
     (2,9),
     (1,6));

  VAR i,k:longint;
      tau:double;
      targetPosition:TVector3;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      tau:=(i/length(Particle))*length(C_IcosahedronEdges);
      k:=trunc(tau);
      tau:=frac(tau);
      k:=k mod length(C_IcosahedronEdges);
      targetPosition:=C_IcosahedronNodes[C_IcosahedronEdges[k,0]]*(1-tau)+
                      C_IcosahedronNodes[C_IcosahedronEdges[k,1]]*(  tau);
    end;
  end;

PROCEDURE TParticleEngine.updateA_wave(CONST progress: double);
  VAR i:longint;
      r:double;
      targetPosition:TVector3;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      r:=sqrt(i/1023)*2;
      targetPosition[0]:=sin(i*FIBFAK)*r;
      targetPosition[2]:=cos(i*FIBFAK)*r;
      targetPosition[1]:=-0.5+0.2*sin(3*r-progress*50);
      a:=accel(v,p,targetPosition,10,-10);
    end;
  end;

PROCEDURE TParticleEngine.updateA_grid(CONST progress: double);
  VAR i:longint;
      k:array[0..2] of longint;
      tgtCol:TVector3;
      targetPosition:TVector3;
  begin
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      k[0]:=round(p[0]*5);
      k[1]:=round(p[1]*5);
      k[2]:=round(p[2]*5);
      tgtCol:=(commonTargetColor+spherePoints[(i*31) and 1023]*0.1);
      if k[lissajousParam[0] mod 3]=0
      then color:=WHITE-tgtCol;

      targetPosition[0]:=k[0]*0.2;
      targetPosition[1]:=k[1]*0.2;
      targetPosition[2]:=k[2]*0.2;
      a:=accel(v,p,targetPosition,20,-100*progress);
    end;
  end;

PROCEDURE TParticleEngine.updateA_lissajous(CONST progress: double);
  VAR i:longint;
      t,tau:double;
      targetPosition:TVector3;
  begin
     t:=progress/5;
     for i:=0 to length(Particle)-1 do with Particle[i] do begin
       tau:=(i/length(Particle)+t)*2*pi;
       targetPosition[0]:=sin(     lissajousParam[0]*tau);
       targetPosition[1]:=sin(pi/6+lissajousParam[1]*tau);
       targetPosition[2]:=sin(pi/3+lissajousParam[2]*tau);
       a:=accel(v,p,targetPosition,20,-1);
     end;
  end;

PROCEDURE TParticleEngine.updateA_vogler(CONST progress: double);
  VAR i,k:longint;
      targetPosition:TVector3;
  begin
    k:=trunc(progress*20);
    k:=((k div 2)*21+13*(k and 1));
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      if i-k>0 then begin
        targetPosition[0]:=sin((i-k)*FIBFAK)*sqrt((i-k)/1023);
        targetPosition[1]:=cos((i-k)*FIBFAK)*sqrt((i-k)/1023);
        targetPosition[2]:=0.2-0.4*sqr((i-k)/1023);
      end else begin
        targetPosition[0]:=0;
        targetPosition[1]:=0;
        targetPosition[2]:=0.2+(i-k)*0.01;
      end;
      a:=accel(v,p,targetPosition,10,-10);
    end;
  end;

PROCEDURE TParticleEngine.updateCol_vogler(CONST progress: double; CONST dt:double);
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
  VAR i,k:longint;
      tau:double;
      targetPosition:TVector3;
      spring:double;
  begin
    k:=0;
    while k<length(Particle) do begin
      tau:=(k/length(Particle)+progress)*2*pi;
      targetPosition[0]:=sin(     lissajousParam[0]*tau);
      targetPosition[2]:=sin(pi/6+lissajousParam[1]*tau);
      targetPosition[1]:=sin(pi/3+lissajousParam[2]*tau);
      spring:=20;
      with Particle[k] do a:=accel(v,p,targetPosition,spring,-10);
      for i:=k+1 to k+31 do with Particle[i] do begin
        targetPosition:=Particle[i-1].p*0.5+targetPosition*0.5;
        spring*=0.99;
        a:=accel(v,p,targetPosition,spring,-10)
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

PROCEDURE TParticleEngine.updateCol_clusters(CONST dt:double);
  VAR i,k:longint;
      col:TVector3;
  begin
    for k:=0 to 11 do begin
      col:=hsvColor(k/11+commonTargetColor[1],1,1);
      for i:=C_clusterChunk[k] to C_clusterChunk[k+1]-1 do with Particle[i] do begin
        color+=(col-color)*0.1*dt;
      end;
    end;
  end;

PROCEDURE TParticleEngine.updateA_sheet(CONST progress: double);
  VAR targetColor:TVector3;
      ix,iy,k,n:longint;
      allPointsCenter:TVector3;
  begin
    allPointsCenter:=ZERO_VECTOR;
    for k:=0 to length(Particle)-1 do allPointsCenter+=Particle[k].p;
    allPointsCenter*=1/length(Particle);

    for ix:=0 to 31 do
    for iy:=0 to 31 do with Particle[ix shl 5 or iy] do begin
   //   targetColor:=commonTargetColor+spherePoints[((ix shl 5 or iy)*31) and 1023]*0.1;
   //   if ((ix and 3)=0) or  ((iy and 3)=0) then targetColor:=WHITE-targetColor;
   //   //color+=(targetColor-color)*(0.1*dt);
   //
   //   targetPosition:=ZERO_VECTOR; n:=0;
   //   if ix> 0 then begin targetPosition+=Particle[((ix+31) and 31) shl 5 or iy].p; n+=1; end;
   //   if ix<31 then begin targetPosition+=Particle[((ix+ 1) and 31) shl 5 or iy].p; n+=1; end;
   //   if iy> 0 then begin targetPosition+=Particle[ix shl 5 or ((iy+31) and 31)].p; n+=1; end;
   //   if iy<31 then begin targetPosition+=Particle[ix shl 5 or ((iy+ 1) and 31)].p; n+=1; end;
   //   targetPosition:=targetPosition*(1/n)-allPointsCenter;
   //   targetPosition*=1/euklideanNorm(targetPosition);
    end;
  end;

PROCEDURE TParticleEngine.updateA_bicyclic(CONST progress: double);
  VAR ix,iy,k:longint;
      allPointsCenter, targetPosition:TVector3;
      r:double;
  begin
    allPointsCenter:=ZERO_VECTOR;
    for k:=0 to length(Particle)-1 do allPointsCenter+=Particle[k].p;
    allPointsCenter*=1/length(Particle);

    for ix:=0 to 31 do
    for iy:=0 to 31 do with Particle[ix shl 5 or iy] do begin
      targetPosition:=(Particle[((ix+ 1) and 31) shl 5 or iy].p+
                       Particle[ix shl 5 or ((iy+ 1) and 31)].p)*0.51-allPointsCenter;
      r:=euklideanNorm(targetPosition);
      if r>2 then targetPosition*=2/r;
    end;
  end;

PROCEDURE TParticleEngine.updateA_clock(CONST progress: double);
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
      q:double;
      targetPosition:TVector3;
      timeString:string;
  begin
    timeString:=FormatDateTime('hhmmss',now);
    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      if i<1008 then begin
        digit:= i div 24;
        bar  := digit mod 7; digit:=digit div 7;
        q    := (i mod 24)/23*24/23;
        targetPosition:=DIGIT_SHIFT[digit]+ BAR_POS[bar,0]*q+BAR_POS[bar,1]*(1-q);
        //if BAR_ACTIVE[timeString[digit+1],bar]
        //then color+=(commonTargetColor    -color)*((digit+1)*dt)
        //else color+=(commonTargetColor*0.3-color)*((digit+1)*dt);
      end else begin
        targetPosition:=POINT_POS[i and 3];
        //color+=(commonTargetColor    -color)*(dt)
      end;
    end;

end;

CONSTRUCTOR TParticleEngine.create;
  VAR i,j: integer;
      acceptAt:double=2;
      accept:boolean;
      temp:TVector3;
  begin

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

    for i:=0 to length(Particle)-1 do with Particle[i] do begin
      p:=randomOnSphere;
      v:=randomOnSphere;
      color[0]:=random;
      color[1]:=random;
      color[2]:=random;
    end;

    attractionMode:=ATTRACTION_MODE_COUNT;
    switchAttractionMode;
  end;

DESTRUCTOR TParticleEngine.destroy;
  begin
    inherited destroy;
  end;

PROCEDURE TParticleEngine.DrawParticles(CONST ParticleList: GLuint);
  VAR i: integer;
  begin
    for i:=0 to length(Particle)-1 do begin
      glColor3f(Particle[i].color[0],Particle[i].color[1],Particle[i].color[2]);
      glPushMatrix;
      glTranslatef(Particle[i].p[0], Particle[i].p[1], Particle[i].p[2]);
      glCallList(ParticleList);
      glPopMatrix;
    end;
  end;

PROCEDURE TParticleEngine.switchAttractionMode;
  CONST primes:array[0..2] of byte=(2,3,5);
  VAR m,p:byte;

      tmp:TParticle;
      i,k:longint;

  begin
    repeat m:=random(ATTRACTION_MODE_COUNT) until m<>attractionMode;

    lissajousParam[0]:=1+random(12);
    lissajousParam[1]:=1+random(12);
    lissajousParam[2]:=1+random(12);

    repeat commonTargetColor:=vectorOf(random,random,random); until commonTargetColor[0]+commonTargetColor[1]+commonTargetColor[2]>1;

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
  end;

end.

