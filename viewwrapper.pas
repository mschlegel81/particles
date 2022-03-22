UNIT viewWrapper;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,
  particlePhysics,vectors,
  GL,OpenGLContext,
  Controls,
  serializationUtil;

TYPE

  { T_viewState }

  T_viewState=object(T_serializable)
    private
      //OpenGL variables
      lightamb,
      lightdif,
      lightpos: array [0..3] of GLfloat;
      ParticleList: GLuint;
      OpenGLControl: TOpenGLControl;
      AreaInitialized: boolean;
      flatShading_:boolean;
      //View rotation:
      rx,ry: single;

      ballRefinement_:byte;
      ballSize_:single;
      hemispheres_:boolean;

      //Frame rate control
      frameCount    : integer;
      LastFrameTicks: integer;
      sleepTimeMilliseconds:double;
      TARGET_FPS:longint;
      TARGET_TICKS_PER_FRAME:double;
      measuredFps:double;
      //Mouse handling
      mouseX,mouseY : longint;
      mouseIsDown   : byte;

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
      PROCEDURE setFlatShading(CONST value:boolean);
      PROCEDURE setHemispheres(CONST value:boolean);
    public
      //Physics time
      modeTicks     : integer;
      ParticleEngine: TParticleEngine;

      CONSTRUCTOR create(control:TOpenGLControl);

      PROPERTY targetFPS:longint read TARGET_FPS write setTargetFPS;
      PROPERTY ballSize:single read ballSize_ write setBallSize;
      PROPERTY ballRefinement:byte read ballRefinement_ write setBallRefinement;
      PROPERTY getFps:double read measuredFps;
      PROPERTY light1Brightness:TGLfloat read getLight1Brightness write setLight1Brightness;
      PROPERTY light2Brightness:TGLfloat read getLight2Brightness write setLight2Brightness;
      PROPERTY flatShading:boolean read flatShading_ write setFlatShading;
      PROPERTY hemispheres:boolean read hemispheres_ write setHemispheres;

      FUNCTION getSerialVersion:dword; virtual;
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

      DESTRUCTOR destroy;
  end;

IMPLEMENTATION

{ T_viewState }

CONSTRUCTOR T_viewState.create(control: TOpenGLControl);
  begin
    OpenGLControl:=control;
    ParticleEngine:=TParticleEngine.create;
    sleepTimeMilliseconds:=0;

    mouseX :=0;
    mouseY :=0;
    mouseIsDown:=0;

    OpenGLControl.OnMouseDown:=@viewMouseDown;
    OpenGLControl.OnMouseMove:=@viewMouseMove;
    OpenGLControl.OnMouseUp  :=@viewMouseUp;
    OpenGLControl.OnResize   :=@viewResize;
    OpenGLControl.OnPaint    :=@viewPaint;

    {ambient color}
    lightamb[0]:=0.2;
    lightamb[1]:=0.2;
    lightamb[2]:=0.2;
    lightamb[3]:=0.0;
    {diffuse color}
    lightdif[0]:=1;
    lightdif[1]:=1;
    lightdif[2]:=1;
    lightdif[3]:=0.0;

    setTargetFPS(60);
    ballRefinement_:=0;
    ballSize_:=0.013;
    flatShading_:=false;
    hemispheres_:=false;
  end;

FUNCTION T_viewState.getSerialVersion: dword;
  begin
    result:=2;
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
    ParticleEngine.saveToStream(stream);
  end;

DESTRUCTOR T_viewState.destroy;
  begin
    FreeAndNil(ParticleEngine);
  end;

PROCEDURE T_viewState.viewMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    mouseX:=x;
    mouseY:=y;
    case button of
      mbLeft : mouseIsDown:=1;
      mbRight: mouseIsDown:=2;
    end;
  end;

PROCEDURE T_viewState.viewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
  begin
    if mouseIsDown<>1 then exit;
    ry+=(x-mouseX)/OpenGLControl.width*180;
    rx+=(y-mouseY)/OpenGLControl.height*180;
    mouseX:=x;
    mouseY:=y;
  end;

PROCEDURE T_viewState.viewMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    if mouseIsDown=2 then ParticleEngine.nextSetup(modeTicks);
    mouseIsDown:=0;
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
          if (commonNormal[2]>-0.05) or not(hemispheres_) then for j:=0 to 2 do begin
            n:=p[j];
            n*=1/euklideanNorm(n);
            if flatShading_
            then glNormal3f(commonNormal[0],commonNormal[1],commonNormal[2])
            else glNormal3f(n[0],n[1],n[2]);
            n*=ballSize_;
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
      //if GLInitialized then exit;
      //GLInitialized:=true;

      {diffuse position}
      n:=vectorOf(1,2,0); n*=1/euklideanNorm(n);
      lightpos[0]:=n[0];
      lightpos[1]:=n[1];
      lightpos[2]:=n[2];
      lightpos[3]:=0.0;

      glLightfv(GL_LIGHT0,GL_AMBIENT ,lightamb);
      if lightamb[0]>0
      then glEnable (GL_LIGHT0)
      else glDisable(GL_LIGHT0);

      glLightfv(GL_LIGHT1,GL_DIFFUSE ,lightdif);
      glLightfv(GL_LIGHT1,GL_POSITION,lightpos);
      if lightdif[0]>0
      then glEnable (GL_LIGHT1)
      else glDisable(GL_LIGHT1);

      glClearColor(0.0,0.0,0.0,0.1);    // sets background color
      glClearDepth(1.0);
      glDepthFunc(GL_LEQUAL);           // the type of depth test to do
      glEnable(GL_DEPTH_TEST);          // enables depth testing
      if flatShading_
      then glShadeModel(GL_FLAT)
      else glShadeModel(GL_SMOOTH);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);

      //glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
      glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);

      glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
      glEnable( GL_BLEND );
      glEnable(GL_COLOR_MATERIAL);
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

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
      tickDelta: integer;

  begin
    inc(frameCount);
    tickDelta:=OpenGLControl.FrameDiffTimeInMSecs;
    inc(modeTicks     ,round(TARGET_TICKS_PER_FRAME));
    inc(LastFrameTicks,tickDelta);
    if (LastFrameTicks>=1000) then begin
      measuredFps:=frameCount*1000/LastFrameTicks;
      dec(LastFrameTicks,1000);
      frameCount:=0;
    end;

    if OpenGLControl.MakeCurrent then begin
      if not AreaInitialized then initializeArea;
      timer:=ParticleEngine.update(modeTicks);

      sleepTimeMilliseconds:=sleepTimeMilliseconds+0.01*(TARGET_TICKS_PER_FRAME-tickDelta);
      if sleepTimeMilliseconds<0 then sleepTimeMilliseconds:=0;
      sleep(trunc(sleepTimeMilliseconds));

      //Update rotation angles
      if (mouseIsDown<>1) then begin
        if ParticleEngine.currentAttractionMode=CLOCK_TARGET
        then ry-=ry*timer
        else ry+=10*timer;
        rx-=rx*timer;

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
      glRotatef(rx,0.1,0.0,0.0);
      glRotatef(ry,0.0,1.0,0.0);
      //Draw
      glLightfv(GL_LIGHT0,GL_POSITION,lightpos);
      glLightfv(GL_LIGHT1,GL_POSITION,lightpos);
      glEnable(GL_BLEND);
      glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
      glMaterialfv(GL_FRONT, GL_SPECULAR, @specular_white);
      glMaterialf(GL_FRONT, GL_SHININESS, 80.0);
      if hemispheres_
      then ParticleEngine.DrawParticles(ParticleList,-rx,-ry)
      else ParticleEngine.DrawParticles(ParticleList,  0,  0);
      glDisable(GL_BLEND);
      glPopMatrix;
      OpenGLControl.SwapBuffers;
    end;
  end;

PROCEDURE T_viewState.setTargetFPS(CONST value: longint);
  begin
    if value<1 then exit;
    TARGET_FPS:=value;
    TARGET_TICKS_PER_FRAME:=1000/value;
  end;

PROCEDURE T_viewState.setBallSize(CONST value: single);
  begin
    if value=ballSize_ then exit;
    ballSize_:=value;
    AreaInitialized:=false;
  end;

PROCEDURE T_viewState.setBallRefinement(CONST value: byte);
  begin
    if value=ballRefinement_ then exit;
    ballRefinement_:=value;
    AreaInitialized:=false;
  end;

FUNCTION T_viewState.getLight1Brightness: TGLfloat;
  begin
    result:=lightamb[0];
  end;

PROCEDURE T_viewState.setLight1Brightness(CONST value: TGLfloat);
  begin
    lightamb[0]:=value;
    lightamb[1]:=value;
    lightamb[2]:=value;
    AreaInitialized:=false;
  end;

FUNCTION T_viewState.getLight2Brightness: TGLfloat;
  begin
    result:=lightdif[0];
  end;

PROCEDURE T_viewState.setLight2Brightness(CONST value: TGLfloat);
  begin
    lightdif[0]:=value;
    lightdif[1]:=value;
    lightdif[2]:=value;
    AreaInitialized:=false;
  end;

PROCEDURE T_viewState.setFlatShading(CONST value: boolean);
  begin
    if value=flatShading_ then exit;
    flatShading_:=value;
    AreaInitialized:=false;
  end;

PROCEDURE T_viewState.setHemispheres(CONST value:boolean);
  begin
    if value=hemispheres_ then exit;
    hemispheres_:=value;
    AreaInitialized:=false;
  end;

end.

