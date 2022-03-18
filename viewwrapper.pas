UNIT viewWrapper;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,
  particlePhysics,vectors,
  GL,OpenGLContext,
  Controls;

TYPE

  { T_viewState }

  T_viewState=class
    private
      //OpenGL variables
      lightamb, lightdif, lightpos: array [0..3] of GLfloat;
      ParticleList: GLuint;
      OpenGLControl: TOpenGLControl;
      AreaInitialized: boolean;
      //View rotation:
      rx,ry: single;

      finerBalls_:boolean;
      ballSize_:single;

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
      //Physics time
      modeTicks     : integer;

      PROCEDURE viewMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE viewMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
      PROCEDURE viewMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE viewResize(Sender: TObject);
      PROCEDURE viewPaint(Sender: TObject);

      PROCEDURE setTargetFPS(CONST value:longint);
      PROCEDURE setBallSize(CONST value:single);
      PROCEDURE SetfinerBalls(CONST value:boolean);
    public
      ParticleEngine: TParticleEngine;

      CONSTRUCTOR create(control:TOpenGLControl);

      PROPERTY targetFPS:longint read TARGET_FPS write setTargetFPS;
      PROPERTY ballSize:single read ballSize_ write setBallSize;
      PROPERTY finerBalls:boolean read finerBalls_ write SetfinerBalls;
      PROPERTY getFps:double read measuredFps;
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

    setTargetFPS(60);
    finerBalls_:=false;
    ballSize_:=0.013;
  end;

PROCEDURE T_viewState.viewMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    mouseX:=x;
    mouseY:=y;
    case button of
      mbLeft : mouseIsDown:=1;
      mbRight: mouseIsDown:=2;
    end;
  end;

PROCEDURE T_viewState.viewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  begin
    if mouseIsDown<>1 then exit;
    ry+=(x-mouseX)/OpenGLControl.width*180;
    rx+=(y-mouseY)/OpenGLControl.height*180;
    mouseX:=x;
    mouseY:=y;
  end;

PROCEDURE T_viewState.viewMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
    CONST C_icosahedronFaces:array[0..19,0..2] of byte=
      ((1,2,0),(0, 4,3),( 5, 6,1),(3, 7, 5),
       (2,9,8),(8,10,4),(11, 9,6),(7,10,11),
       (0,3,1),(1, 3,5),( 9,10,8),(11,10,9),
       (2,8,0),(0, 8,4),( 5,11,6),( 7,11,5),
       (1,6,2),(4, 7,3),( 2, 6,9),(10, 7,4));
       //      2
       //     / \
       //    /   \
       //   5-----4
       //  / \   / \
       // /   \ /   \
       //0-----3-----1
       FINER_NODE:array[0..11] of byte = (0,3,5,3,1,4,3,4,5,5,4,2);
    VAR p:array[0..5] of TVector3;
        n:TVector3;
        i,j:longint;
    begin
      //if GLInitialized then exit;
      //GLInitialized:=true;

      {setting lighting conditions}
      {ambient color}
      lightamb[0]:=0.2;
      lightamb[1]:=0.2;
      lightamb[2]:=0.2;
      lightamb[3]:=1.0;
      {diffuse color}
      lightdif[0]:=1;
      lightdif[1]:=1;
      lightdif[2]:=1;
      lightdif[3]:=1.0;
      {diffuse position}
      n:=vectorOf(1,2,0); n*=1/euklideanNorm(n);
      lightpos[0]:=n[0];
      lightpos[1]:=n[1];
      lightpos[2]:=n[2];
      lightpos[3]:=0.0;

      glLightfv(GL_LIGHT0,GL_AMBIENT ,lightamb);
      glLightfv(GL_LIGHT1,GL_DIFFUSE ,lightdif);
      glLightfv(GL_LIGHT1,GL_POSITION,lightpos);

      glEnable(GL_LIGHT0);
      glEnable(GL_LIGHT1);

      glClearColor(0.0,0.0,0.0,0.1);    // sets background color
      glClearDepth(1.0);
      glDepthFunc(GL_LEQUAL);           // the type of depth test to do
      glEnable(GL_DEPTH_TEST);          // enables depth testing
      glShadeModel(GL_SMOOTH);          // enables smooth color shading
      glColor4f(0.7,0.7,0.7,1.0);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
      glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
      glEnable( GL_BLEND );
      glEnable(GL_COLOR_MATERIAL);
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
      glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);

      ParticleList:=glGenLists(1);

      glColor3f(1,0.5,0);
      glNewList(ParticleList, GL_COMPILE);
        glBegin(GL_TRIANGLES);
          for i:=0 to 19 do begin
            if finerBalls_ then begin
              p[0]:=C_IcosahedronNodes[C_icosahedronFaces[i,0]];
              p[1]:=C_IcosahedronNodes[C_icosahedronFaces[i,1]];
              p[2]:=C_IcosahedronNodes[C_icosahedronFaces[i,2]];
              p[3]:=p[0]+p[1];
              p[4]:=p[1]+p[2];
              p[5]:=p[2]+p[0];
              for j:=0 to 5 do p[j]*=1/euklideanNorm(p[j]);
              for j:=0 to 11 do begin
                n:=p[FINER_NODE[j]];
                glNormal3f(n[0],n[1],n[2]);
                n*=ballSize_;
                glVertex3f(n[0],n[1],n[2]);
              end;
            end else begin
              for j:=0 to 2 do begin
                n:=C_IcosahedronNodes[C_icosahedronFaces[i,j]];
                n*=1/euklideanNorm(n);
                glNormal3f(n[0],n[1],n[2]);
                n*=ballSize_;
                glVertex3f(n[0],n[1],n[2]);
              end;
            end;
          end;

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

  CONST az=1;
  VAR ax:double=az;
      ay:double=az;
      timer:single;
      tickDelta: integer;

  begin
    inc(frameCount);
    tickDelta:=OpenGLControl.FrameDiffTimeInMSecs;
    inc(modeTicks     ,tickDelta);
    inc(LastFrameTicks,tickDelta);
    if (LastFrameTicks>=1000) then begin
      measuredFps:=frameCount*1000/LastFrameTicks;
      dec(LastFrameTicks,1000);

//      caption:='Particles ('+intToStr(frameCount)+'fps)';
//      DebugLn(['TExampleForm.OpenGLControl1Paint Frames per second: ',measuredFps]);
      frameCount:=0;
    end;

    if OpenGLControl.MakeCurrent then begin
      if not AreaInitialized then initializeArea;
      timer:=ParticleEngine.update(modeTicks);

      sleepTimeMilliseconds:=sleepTimeMilliseconds+0.1*(TARGET_TICKS_PER_FRAME-tickDelta);
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
      glLightfv(GL_LIGHT1,GL_POSITION,lightpos);
      glEnable(GL_BLEND);
      ParticleEngine.DrawParticles(ParticleList);
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

PROCEDURE T_viewState.setFinerBalls(CONST value: boolean);
  begin
    if value=finerBalls_ then exit;
    finerBalls_:=value;
    AreaInitialized:=false;
  end;

end.

