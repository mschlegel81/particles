UNIT viewWrapper;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,
  particlePhysics,vectors,
  GL,OpenGLContext,
  Controls,
  serializationUtil,
  EpikTimer;

TYPE
  T_viewState=object(T_serializable)
    private
      OpenGLControl: TOpenGLControl;
      AreaInitialized: boolean;
      lighting:record
        ambient,
        diffuse,
        specular,
        position: array [0..3] of GLfloat;
      end;
      geometry:record
        flatShading_:boolean;
        ballRefinement_:byte;
        ballSize_:single;
        hemispheres_:boolean;
        ParticleList: GLuint;
      end;
      rotation:record
        rx,ry: single;
        lockX,lockY:boolean;
      end;
      //Frame rate control
      frameRateControl:record
        frameTimer            : TEpikTimer;
        frameCount            : integer;
        LastFrameTicks        : double;
        averageFrameTicks     : double;
        sleepTimeMilliseconds : double;
        TARGET_FPS            : longint;
        TARGET_TICKS_PER_FRAME: double;
        measuredFps:double;
      end;
      //Mouse handling
      mouse:record
        mouseX,
        mouseY : longint;
        isDown : (NO,leftDown,rightDown);
      end;

      PROCEDURE viewMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE viewMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
      PROCEDURE viewMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE viewResize(Sender: TObject);
      PROCEDURE viewPaint(Sender: TObject);

      PROCEDURE setTargetFPS(CONST value:longint);
      PROCEDURE setBallSize(CONST value:single);
      PROCEDURE setBallRefinement(CONST value:byte);
      FUNCTION getLight1Brightness:TGLfloat;
      PROCEDURE setLight1Brightness(CONST value:TGLfloat);
      FUNCTION getLight2Brightness:TGLfloat;
      PROCEDURE setLight2Brightness(CONST value:TGLfloat);
      FUNCTION getLight3Brightness:TGLfloat;
      PROCEDURE setLight3Brightness(CONST value:TGLfloat);
      PROCEDURE setFlatShading(CONST value:boolean);
      PROCEDURE setHemispheres(CONST value:boolean);
    public
      //Physics time
      modeTicks     : double;
      ParticleEngine: TParticleEngine;

      CONSTRUCTOR create(control:TOpenGLControl);

      PROPERTY targetFPS:longint read frameRateControl.TARGET_FPS write setTargetFPS;
      PROPERTY ballSize:single read geometry.ballSize_ write setBallSize;
      PROPERTY ballRefinement:byte read geometry.ballRefinement_ write setBallRefinement;
      PROPERTY getFps:double read frameRateControl.measuredFps;
      PROPERTY light1Brightness:TGLfloat read getLight1Brightness write setLight1Brightness;
      PROPERTY light2Brightness:TGLfloat read getLight2Brightness write setLight2Brightness;
      PROPERTY light3Brightness:TGLfloat read getLight3Brightness write setLight3Brightness;
      PROPERTY flatShading:boolean read geometry.flatShading_ write setFlatShading;
      PROPERTY hemispheres:boolean read geometry.hemispheres_ write setHemispheres;

      PROPERTY lockXRotation: boolean read rotation.lockX write rotation.lockX;
      PROPERTY lockYRotation: boolean read rotation.lockY write rotation.lockY;

      FUNCTION getSerialVersion:dword; virtual;
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

      DESTRUCTOR destroy;
  end;

IMPLEMENTATION
USES LCLProc;
CONSTRUCTOR T_viewState.create(control: TOpenGLControl);
  begin
    OpenGLControl:=control;
    ParticleEngine:=TParticleEngine.create;
    with frameRateControl do begin
      sleepTimeMilliseconds:=0;
      averageFrameTicks:=0; //Expect first frame to take no time at all
      frameTimer:=TEpikTimer.create(nil);
      frameTimer.clear;
    end;

    with mouse do begin
      mouseX:=0;
      mouseY:=0;
      isDown:=NO;
    end;

    OpenGLControl.OnMouseDown:=@viewMouseDown;
    OpenGLControl.OnMouseMove:=@viewMouseMove;
    OpenGLControl.OnMouseUp  :=@viewMouseUp;
    OpenGLControl.OnResize   :=@viewResize;
    OpenGLControl.OnPaint    :=@viewPaint;

    with lighting do begin
      {ambient color}
      ambient[0]:=0.2;
      ambient[1]:=0.2;
      ambient[2]:=0.2;
      ambient[3]:=0.0;
      {diffuse color}
      diffuse[0]:=1;
      diffuse[1]:=1;
      diffuse[2]:=1;
      diffuse[3]:=0.0;
      {diffuse color}
      specular[0]:=1;
      specular[1]:=1;
      specular[2]:=1;
      specular[3]:=0.0;
    end;

    with geometry do begin
      ballRefinement_:=0;
      ballSize_:=0.013;
      flatShading_:=false;
      hemispheres_:=false;
    end;
    setTargetFPS(60);
  end;

FUNCTION T_viewState.getSerialVersion: dword;
  begin
    result:=4;
  end;

FUNCTION T_viewState.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    if not inherited then exit(false);
    ballSize:=stream.readSingle;
    ballRefinement:=stream.readByte([0,1,2,3]);
    flatShading:=stream.readBoolean;
    hemispheres:=stream.readBoolean;
    setTargetFPS(stream.readLongint);
    light1Brightness:=stream.readSingle;
    light2Brightness:=stream.readSingle;
    light3Brightness:=stream.readSingle;
    with rotation do begin
      lockX:=stream.readBoolean;
      if lockX then rx:=stream.readSingle;
      lockY:=stream.readBoolean;
      if lockY then ry:=stream.readSingle;
    end;
    result:=ParticleEngine.loadFromStream(stream) and stream.allOkay;
  end;

PROCEDURE T_viewState.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    inherited;
    stream.writeSingle(ballSize);
    stream.writeByte(ballRefinement);
    stream.writeBoolean(flatShading);
    stream.writeBoolean(hemispheres);
    stream.writeLongint(targetFPS);
    stream.writeSingle(light1Brightness);
    stream.writeSingle(light2Brightness);
    stream.writeSingle(light3Brightness);
    with rotation do begin
      stream.writeBoolean(lockX);
      if lockX then stream.writeSingle(rx);
      stream.writeBoolean(lockY);
      if lockY then stream.writeSingle(ry);
    end;
    ParticleEngine.saveToStream(stream);
  end;

DESTRUCTOR T_viewState.destroy;
  begin
    FreeAndNil(frameRateControl.frameTimer);
    ParticleEngine.destroy;
  end;

PROCEDURE T_viewState.viewMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    with mouse do begin
      mouseX:=x;
      mouseY:=y;
      case button of
        mbLeft : isDown:=leftDown;
        mbRight: isDown:=rightDown;
      end;
    end;
  end;

PROCEDURE T_viewState.viewMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
  begin
    if mouse.isDown<>leftDown then exit;
    with mouse do begin
      rotation.ry+=(x-mouseX)/OpenGLControl.width*180;
      rotation.rx+=(y-mouseY)/OpenGLControl.height*180;
      mouseX:=x;
      mouseY:=y;
    end;
  end;

PROCEDURE T_viewState.viewMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    if mouse.isDown=rightDown then ParticleEngine.nextSetup(modeTicks);
    mouse.isDown:=NO;
  end;

PROCEDURE T_viewState.viewResize(Sender: TObject);
  begin
    if (AreaInitialized) and OpenGLControl.MakeCurrent then
      glViewport (0, 0, OpenGLControl.width, OpenGLControl.height);
  end;

PROCEDURE T_viewState.viewPaint(Sender: TObject);
  PROCEDURE initializeArea;
    PROCEDURE addFace(CONST p0,p1,p2:TVector3; CONST refine:byte);
      CONST FINER_NODE:array[0..3,0..2] of byte = ((0,3,5),(3,1,4),(3,4,5),(5,4,2));
      //      2
      //     / \
      //    /   \
      //   5-----4
      //  / \   / \
      // /   \ /   \
      //0-----3-----1
      VAR p:array[0..5] of TVector3;
          n,commonNormal:TVector3;
          j:longint;
      begin
        p[0]:=p0*(1/euklideanNorm(p0));
        p[1]:=p1*(1/euklideanNorm(p1));
        p[2]:=p2*(1/euklideanNorm(p2));
        if refine=0 then begin
          commonNormal:=p[0]+p[1]+p[2];
          commonNormal*=1/euklideanNorm(commonNormal);
          if (commonNormal[2]>-0.05) or not(geometry.hemispheres_) then for j:=0 to 2 do begin
            n:=p[j];
            n*=1/euklideanNorm(n);
            if geometry.flatShading_
            then glNormal3f(commonNormal[0],commonNormal[1],commonNormal[2])
            else glNormal3f(n[0],n[1],n[2]);
            n*=geometry.ballSize_;
            glVertex3f(n[0],n[1],n[2]);
          end;
        end else begin
          p[3]:=p[0]+p[1];
          p[4]:=p[1]+p[2];
          p[5]:=p[2]+p[0];
          for j:=0 to 3 do addFace(p[FINER_NODE[j,0]],p[FINER_NODE[j,1]],p[FINER_NODE[j,2]],refine-1);
        end;
      end;

    CONST C_icosahedronFaces:array[0..19,0..2] of byte=
      ((1,2,0),(0, 4,3),( 5, 6,1),(3, 7, 5),
       (2,9,8),(8,10,4),(11, 9,6),(7,10,11),
       (0,3,1),(1, 3,5),( 9,10,8),(11,10,9),
       (2,8,0),(0, 8,4),( 5,11,6),( 7,11,5),
       (1,6,2),(4, 7,3),( 2, 6,9),(10, 7,4));

    VAR n:TVector3;
        i:longint;
    begin
      {diffuse position}
      n:=vectorOf(1,2,0); n*=1/euklideanNorm(n);
      with lighting do begin
        position[0]:=n[0];
        position[1]:=n[1];
        position[2]:=n[2];
        position[3]:=0.0;
        glLightfv(GL_LIGHT0,GL_AMBIENT ,ambient);
        glLightfv(GL_LIGHT0,GL_DIFFUSE ,diffuse);
        glLightfv(GL_LIGHT0,GL_SPECULAR,specular);
        glEnable (GL_LIGHT0);

        glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
        glEnable( GL_BLEND );
        glEnable(GL_COLOR_MATERIAL);
        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        glClearColor(0.0,0.0,0.0,1.0);
      end;

      glClearDepth(1.0);
      glDepthFunc(GL_LEQUAL);           // the type of depth test to do
      glEnable(GL_DEPTH_TEST);          // enables depth testing
      if geometry.flatShading_
      then glShadeModel(GL_FLAT)
      else glShadeModel(GL_SMOOTH);
      glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

      with geometry do begin
        ParticleList:=glGenLists(1);
        glColor3f(1,0.5,0);
        glNewList(ParticleList, GL_COMPILE);
          glBegin(GL_TRIANGLES);
            for i:=0 to 19 do
              addFace(C_IcosahedronNodes[C_icosahedronFaces[i,0]],
                      C_IcosahedronNodes[C_icosahedronFaces[i,1]],
                      C_IcosahedronNodes[C_icosahedronFaces[i,2]],
                      ballRefinement_);
           glEnd;
        glEndList;
        glEnable(GL_LIGHTING);
      end;

      glMatrixMode (GL_PROJECTION);
      glLoadIdentity ();
      glFrustum (-0.1, 0.1, -0.1, 0.1, 0.35, 20.0);
      glMatrixMode (GL_MODELVIEW);
      glViewport (0, 0, OpenGLControl.width, OpenGLControl.height);
      AreaInitialized:=true;
    end;

  CONST specular_white:array[0..3] of GLfloat=(1,1,1,1);
  CONST az=1;
  VAR ax:double=az;
      ay:double=az;
      timer:single;
      tickDelta: double;
      sleepMs:longint;
  begin
    with frameRateControl do begin
      tickDelta:=frameTimer.elapsed*1000;
      frameTimer.clear;
      frameTimer.start;
      averageFrameTicks:=averageFrameTicks*0.9+tickDelta*0.1;
      modeTicks        +=averageFrameTicks;
      //Frame rate counting:
      inc(frameCount);
      LastFrameTicks+=tickDelta;
      if (LastFrameTicks>=1000) then begin
        measuredFps:=1E3*frameCount/LastFrameTicks;
        LastFrameTicks-=1000;
        frameCount:=0;
      end;
    end;

    if OpenGLControl.MakeCurrent then begin
      if not AreaInitialized then initializeArea;
      timer:=ParticleEngine.update(modeTicks);

      with frameRateControl do begin
        sleepTimeMilliseconds+=TARGET_TICKS_PER_FRAME-averageFrameTicks;
        if sleepTimeMilliseconds<0 then sleepTimeMilliseconds:=0;
        sleepMs:=trunc(sleepTimeMilliseconds);
        if sleepMs>0 then begin
          sleepTimeMilliseconds-=sleepMs;
          sleep(sleepMs);
          DebugLn(['ST ',sleepMs,' rest: ',sleepTimeMilliseconds,' Timing: ',tickDelta,' ',averageFrameTicks,' ',TARGET_TICKS_PER_FRAME]);
        end;
      end;

      //Update rotation angles
      if (mouse.isDown<>leftDown) then with rotation do begin
        if not(lockY) then begin
          if ParticleEngine.currentAttractionMode=CLOCK_TARGET
          then ry-=ry*timer
          else ry+=10*timer;
        end;
        if not(lockX)
        then rx-=rx*timer;

        if ry> 180 then ry-=360;
        if ry<-180 then ry+=360;
        if rx> 180 then rx-=360;
        if rx<-180 then rx+=360;
      end;

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      glLoadIdentity;
      glPushMatrix;
      //Scale to preserve aspect ratio
      if OpenGLControl.width>OpenGLControl.height
      then ax*=OpenGLControl.height/OpenGLControl.width
      else ay*=OpenGLControl.width/OpenGLControl.height;
      glTranslatef(0,0,-5);
      glScalef(ax,ay,az);
      //Rotate
      glRotatef(rotation.rx,1.0,0.0,0.0);
      glRotatef(rotation.ry,0.0,1.0,0.0);
      //Draw
      glLightfv(GL_LIGHT0,GL_POSITION,lighting.position);
      glEnable(GL_BLEND);
      glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
      glMaterialfv(GL_FRONT, GL_SPECULAR, @specular_white);
      glMaterialf(GL_FRONT, GL_SHININESS, 80.0);
      if geometry.hemispheres_
      then ParticleEngine.DrawParticles(geometry.ParticleList,-rotation.rx,-rotation.ry)
      else ParticleEngine.DrawParticles(geometry.ParticleList,           0,           0);
      glDisable(GL_BLEND);
      glPopMatrix;
      OpenGLControl.SwapBuffers;
    end;
  end;

PROCEDURE T_viewState.setTargetFPS(CONST value: longint);
  begin
    if value>=1 then with frameRateControl do begin
      TARGET_FPS:=value;
      if TARGET_FPS>100
      then TARGET_TICKS_PER_FRAME:=0
      else TARGET_TICKS_PER_FRAME:=1000/value;
      sleepTimeMilliseconds:=0;
    end;
  end;

PROCEDURE T_viewState.setBallSize(CONST value: single);
  begin
    if value=geometry.ballSize_ then exit;
    geometry.ballSize_:=value;
    AreaInitialized:=false;
  end;

PROCEDURE T_viewState.setBallRefinement(CONST value: byte);
  begin
    if value=geometry.ballRefinement_ then exit;
    geometry.ballRefinement_:=value;
    AreaInitialized:=false;
  end;

FUNCTION T_viewState.getLight1Brightness: TGLfloat;
  begin
    result:=lighting.ambient[0];
  end;

PROCEDURE T_viewState.setLight1Brightness(CONST value: TGLfloat);
  begin
    with lighting do begin
      ambient[0]:=value;
      ambient[1]:=value;
      ambient[2]:=value;
    end;
    AreaInitialized:=false;
  end;

FUNCTION T_viewState.getLight2Brightness: TGLfloat;
  begin
    result:=lighting.diffuse[0];
  end;

PROCEDURE T_viewState.setLight2Brightness(CONST value: TGLfloat);
  begin
    with lighting do begin
      diffuse[0]:=value;
      diffuse[1]:=value;
      diffuse[2]:=value;
    end;
    AreaInitialized:=false;
  end;

FUNCTION T_viewState.getLight3Brightness: TGLfloat;
  begin
    result:=lighting.specular[0];
  end;

PROCEDURE T_viewState.setLight3Brightness(CONST value: TGLfloat);
  begin
    with lighting do begin
      specular[0]:=value;
      specular[1]:=value;
      specular[2]:=value;
    end;
    AreaInitialized:=false;
  end;

PROCEDURE T_viewState.setFlatShading(CONST value: boolean);
  begin
    if value=geometry.flatShading_ then exit;
    geometry.flatShading_:=value;
    AreaInitialized:=false;
  end;

PROCEDURE T_viewState.setHemispheres(CONST value:boolean);
  begin
    if value=geometry.hemispheres_ then exit;
    geometry.hemispheres_:=value;
    AreaInitialized:=false;
  end;

end.

