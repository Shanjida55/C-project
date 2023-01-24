#include<windows.h>
#include<mmsystem.h>
#include<GL/glut.h>
#define PI 3.1416
#include<math.h>
#include<cstdio>
#include <GL/gl.h>


    GLfloat position = 0.0f;
    GLfloat speed = 0.08f;
    GLfloat position1 = 0.0f;
    GLfloat speed1 = 0.05f;
    //GLfloat position2 = 0.0f;
    //GLfloat speed2 = 0.01f;
    GLfloat position3 = 0.0f;
    GLfloat speed3 = 0.02f;
    GLfloat position4 = 0.0f;
    GLfloat speed4 = 0.03f;
    GLfloat position5 = 0.0f;
    GLfloat speed5 = 0.02f;
    GLfloat position6 = 0.0f;
    GLfloat speed6 = 0.01f;


    void update(int value) {//streamer
    if(position <-1.0)
        position =  1.0f;
    position -= speed;
    glutPostRedisplay();
    glutTimerFunc(100, update, 0);
    }
    void update1(int value1) {//bird
    if(position1 >1.0)
        position1 = -1.0f;
    position1 += speed1;
    glutPostRedisplay();
    glutTimerFunc(100, update1, 0);
    }

    /*void update2(int value2) {//sun
    if(position2 <-0.30)
        position2 = 1.0f;
    position2 -= speed2;
    glutPostRedisplay();
    glutTimerFunc(100, update2, 0);
    }*/
    void update3(int value3) {//boat
    if(position3 <-1.0)
        position3 =  1.0f;
    position3 -= speed3;
    glutPostRedisplay();
    glutTimerFunc(100, update3, 0);
    }
    void update4(int value4) {//sky1
    if(position4 >1.0)
        position4 = -1.0f;
    position4 += speed4;
    glutPostRedisplay();
    glutTimerFunc(100, update4, 0);
    }
    void update5(int value5) {//sky 2
    if(position5 >1.0)
        position5 = -1.0f;
    position5 += speed5;
    glutPostRedisplay();
    glutTimerFunc(100, update5, 0);
    }
    void update6(int value6) {//sky3
    if(position6 >1.0)
        position6 = -1.0f;
    position6 += speed6;
    glutPostRedisplay();
    glutTimerFunc(100, update6, 0);
    }
    void display(){
    glClearColor(1.0f, 1.0f, 1.0f,1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glLineWidth(2);

    glBegin(GL_QUADS);//GROUND
    glColor3f(0.67f, 0.87f, 0.0f);
    glVertex2f(-1.0f, -1.0f);
    glVertex2f(1.0f, -1.0f);
    glVertex2f(1.0f, 0.55f);
    glVertex2f(-1.0f, 0.55f);
    glEnd();

    glBegin(GL_QUADS);// SKY
    glColor3f(.68f,.85f,.90f);
    glVertex2f(-1.0f, 0.55f);
    glColor3f(.68f,.85f,.90f);
    glVertex2f(1.0f, 0.55f);
    glColor3f(.0f,.70f,.93f);
    glVertex2f(1.0f, 1.0f);
    glColor3f(.0f,.70f,.93f);
    glVertex2f(-1.0f, 1.0f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(22, 82, 23);
    glVertex2f(-0.50f, 0.55f);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.40f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(22, 82, 23);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(-0.20f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(22, 82, 23);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(0.10f, 0.55f);
    glVertex2f(0.0f, 0.65f);
    glEnd();



    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-1.0f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.90f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.70f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.50f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(-0.30f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(-0.10f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(0.10f, 0.65f);
    glEnd();


    //glPushMatrix();
    //glTranslatef(0.0f, position2, 0.0f);
    int x;//sun
    GLfloat p10 = 0.78f; GLfloat q10 = 0.90f;
    GLfloat r10 = 0.10f;
    int triangle10 = 40;
    GLfloat tp10 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    //glColor3ub(222, 82, 26);
    glColor3ub(255, 255, 0);
    glVertex2f(p10,q10);
    for(x = 0; x <= triangle10; x++){
        glVertex2f(p10+(r10*cos(x*tp10/triangle10)), q10+(r10*sin(x*tp10/triangle10)));
    }

    glEnd();
       // glPopMatrix();


    glBegin(GL_POLYGON);//river
    //glColor3f(0.51f, 0.44f, 1.0f);

    glColor3f(0.40f, 0.71f, 1.0f);
    glVertex2f(-1.0f, 0.10f);
    glVertex2f(1.0f, 0.10f);

    //glColor3ub(32, 76, 140);
    glColor3f(0.0f, 0.60f, 0.88f);
    glVertex2f(1.0f, 0.55f);
    glColor3f(0.0f, 0.60f, 0.88f);
    glVertex2f(-1.0f, 0.55f);
    glEnd();


    glPushMatrix();
    glTranslatef(position,0.0f, 0.0f);
    glBegin(GL_QUADS);// streamer ist part
    glColor3f(0.36f, 0.20f, 0.09f);
    glVertex2f(0.70f, 0.125f);
    glVertex2f(0.90f, 0.125f);
    glVertex2f(0.95f, 0.175f);
    glVertex2f(0.65f, 0.175f);
    glEnd();

    glBegin(GL_QUADS);//2nd part
    glColor3f(0.0f, 0.45f, 0.26f);
    glVertex2f(0.70f, 0.175f);
    glVertex2f(0.90f, 0.175f);
    glVertex2f(0.90f, 0.225f);
    glVertex2f(0.70f, 0.225f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.725f, 0.1875f);
    glVertex2f(0.75f, 0.1875f);
    glVertex2f(0.75f, 0.2125f);
    glVertex2f(0.725f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.775f, 0.1875f);
    glVertex2f(0.80f, 0.1875f);
    glVertex2f(0.80f, 0.2125f);
    glVertex2f(0.775f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.825f, 0.1875f);
    glVertex2f(0.85f, 0.1875f);
    glVertex2f(0.85f, 0.2125f);
    glVertex2f(0.825f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.875f, 0.1875f);
    glVertex2f(0.90f, 0.1875f);
    glVertex2f(0.90f, 0.2125f);
    glVertex2f(0.875f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);//3rd part
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.725f, 0.225f);
    glVertex2f(0.875f, 0.225f);
    glVertex2f(0.875f, 0.255f);
    glVertex2f(0.725f, 0.255f);
    glEnd();

    glBegin(GL_QUADS);// air out
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.745f, 0.255f);
    glVertex2f(0.760f, 0.255f);
    glVertex2f(0.760f, 0.305f);
    glVertex2f(0.745f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);// air out
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.775f, 0.255f);
    glVertex2f(0.79f, 0.255f);
    glVertex2f(0.79f, 0.305f);
    glVertex2f(0.775f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);// air out
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.805f, 0.255f);
    glVertex2f(0.82f, 0.255f);
    glVertex2f(0.82f, 0.305f);
    glVertex2f(0.805f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);// air out
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.835f, 0.255f);
    glVertex2f(0.85f, 0.255f);
    glVertex2f(0.85f, 0.305f);
    glVertex2f(0.835f, 0.305f);
    glEnd();

    glPopMatrix();


    glPushMatrix();
    glTranslatef(position3,0.0f, 0.0f);
    glBegin(GL_QUADS);//boat
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.47f, 0.325f);
    glVertex2f(0.65f, 0.325f);
    glVertex2f(0.70f, 0.375f);
    glVertex2f(0.42f, 0.375f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.49f, 0.375f);
    glVertex2f(0.49f, 0.475f);

    glVertex2f(0.50f, 0.375f);
    glVertex2f(0.50f, 0.525f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.55f, 0.375f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.57f, 0.375f);

    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.455f, 0.38f);
    glVertex2f(0.49f, 0.38f);
    glVertex2f(0.49f, 0.475f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.50f, 0.38f);
    glVertex2f(0.535f, 0.38f);
    glVertex2f(0.50f, 0.575f);
    glEnd();

    glBegin(GL_TRIANGLES);//maji
    glColor3ub(21, 176, 34);
    glVertex2f(0.63f, 0.40f);
    glVertex2f(0.67f, 0.40f);
    glVertex2f(0.65f, 0.45f);
    glEnd();

    int v;//maji

    GLfloat p24=0.65f; GLfloat q24= 0.45f; GLfloat r24 = 0.01f;
    int Gtringle24=40;

    GLfloat tp24 =2.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(207, 202, 155);
    glVertex2f (p24,q24);
    for(v= 0;v<=Gtringle24; v++)
    {
    glVertex2f (
    p24+(r24*cos(v*tp24/Gtringle24)),
    q24+(r24*sin(v*tp24/Gtringle24))
    );
    }
    glEnd ();

    glBegin(GL_LINES);//maji
    glColor3ub(207, 202, 155);
    glVertex2f(0.645f, 0.40f);
    glVertex2f(0.645f, 0.375f);

    glVertex2f(0.655f, 0.40f);
    glVertex2f(0.655f, 0.375f);

    glVertex2f(0.66f, 0.425f);
    glVertex2f(0.62f, 0.415f);

    glVertex2f(0.64f, 0.425f);
    glVertex2f(0.61f, 0.435f);

    glColor3ub(145, 86, 3);
    glVertex2f(0.61f, 0.445f);
    glVertex2f(0.66f, 0.30f);

    glEnd();
        glPopMatrix();


    glBegin(GL_QUADS);//Mosque
    glColor3f(0.74f, 0.45f, 0.26f);
    glVertex2f(-0.75f, -0.20f);
    glVertex2f(0.20f, -0.20f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(-0.75f, 0.50f);
    glEnd();

    glBegin(GL_QUADS);//Mosque upper border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.75f, 0.50f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(0.20f, 0.515f);
    glVertex2f(-0.75f, 0.515f);
    glEnd();

    glBegin(GL_QUADS);//Road
    glColor3f(0.97f, 0.98f, 1.0f);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.45f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow left
    glColor3f(0.27f, 0.55f, 0.0f);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(-0.65f, -1.0f);
    glVertex2f(-0.45f, -0.20f);
    glVertex2f(-0.47f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow right
    glColor3f(0.27f, 0.55f, 0.0f);
    glVertex2f(0.15f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.12f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Mosque bottom border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, -0.25f);
    glVertex2f(0.275f, -0.25f);
    glVertex2f(0.275f, -0.20f);
    glVertex2f(-0.825f, -0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.35f, -0.20f);
    glVertex2f(-0.20f, -0.20f);
    glVertex2f(-0.20f, 0.20f);
    glVertex2f(-0.225f, 0.30f);
    glVertex2f(-0.275f, 0.35f);
    glVertex2f(-0.325f, 0.30f);
    glVertex2f(-0.35f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.325f, -0.20f);
    glVertex2f(-0.21f, -0.20f);
    glVertex2f(-0.21f, 0.20f);
    glVertex2f(-0.228f, 0.28f);
    glVertex2f(-0.275f, 0.32f);
    glVertex2f(-0.31f, 0.28f);
    glVertex2f(-0.325f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.15f, -0.20f);
    glVertex2f(-0.05f, -0.20f);
    glVertex2f(-0.05f, 0.15f);
    glVertex2f(-0.075f, 0.2125f);
    glVertex2f(-0.10f, 0.25f);
    glVertex2f(-0.125f, 0.2125f);
    glVertex2f(-0.15f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.135f, -0.20f);
    glVertex2f(-0.058f, -0.20f);
    glVertex2f(-0.058f, 0.15f);
    glVertex2f(-0.085f, 0.2125f);
    glVertex2f(-0.10f, 0.225f);
    glVertex2f(-0.112f, 0.2125f);
    glVertex2f(-0.135f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.0f, -0.20f);
    glVertex2f(0.10f, -0.20f);
    glVertex2f(0.10f, 0.15f);
    glVertex2f(0.075f, 0.2125f);
    glVertex2f(0.050f, 0.25f);
    glVertex2f(0.025f, 0.2125f);
    glVertex2f(0.0f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.015f, -0.20f);
    glVertex2f(0.092f, -0.20f);
    glVertex2f(0.092f, 0.15f);
    glVertex2f(0.065f, 0.2125f);
    glVertex2f(0.050f, 0.225f);
    glVertex2f(0.037f, 0.2125f);
    glVertex2f(0.015f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.50f, -0.20f);
    glVertex2f(-0.40f, -0.20f);
    glVertex2f(-0.40f, 0.15f);
    glVertex2f(-0.425f, 0.2125f);
    glVertex2f(-0.45f, 0.25f);
    glVertex2f(-0.475f, 0.2125f);
    glVertex2f(-0.50f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.492f, -0.20f);
    glVertex2f(-0.415f, -0.20f);
    glVertex2f(-0.4155f, 0.15f);
    glVertex2f(-0.437f, 0.2125f);
    glVertex2f(-0.45f, 0.225f);
    glVertex2f(-0.465f, 0.2125f);
    glVertex2f(-0.492f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.65f, -0.20f);
    glVertex2f(-0.55f, -0.20f);
    glVertex2f(-0.55f, 0.15f);
    glVertex2f(-0.575f, 0.2125f);
    glVertex2f(-0.60f, 0.25f);
    glVertex2f(-0.625f, 0.2125f);
    glVertex2f(-0.65f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.642f, -0.20f);
    glVertex2f(-0.565f, -0.20f);
    glVertex2f(-0.565f, 0.15f);
    glVertex2f(-0.587f, 0.2125f);
    glVertex2f(-0.60f, 0.225f);
    glVertex2f(-0.615f, 0.2125f);
    glVertex2f(-0.642f, 0.15f);
    glEnd();


    glBegin(GL_QUADS);//left minar
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.80f, -0.20f);
    glVertex2f(-0.70f, -0.20f);
    glVertex2f(-0.70f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//left miner border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.25f);
    glVertex2f(-0.675f, 0.25f);
    glVertex2f(-0.675f, 0.30f);
    glVertex2f(-0.825f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.525f);
    glVertex2f(-0.675f, 0.525f);
    glVertex2f(-0.675f, 0.55f);
    glVertex2f(-0.825f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.78f, 0.30f);
    glVertex2f(-0.72f, 0.30f);
    glVertex2f(-0.72f, 0.45f);
    glVertex2f(-0.75f, 0.475f);
    glVertex2f(-0.78f, 0.45f);
    glEnd();

    int t;// Right 2nd gombuj

    GLfloat x7=0.11f; GLfloat h7= 0.515f; GLfloat Gr5 = 0.07f;
    int Gtringle25=40;

    GLfloat tp25 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x7,h7);
    for(t= 0;t<=Gtringle25; t++)
    {
    glVertex2f (
    x7+(Gr5*cos(t*tp25/Gtringle25)),
    h7+(Gr5*sin(t*tp25/Gtringle25))
    );
    }
    glEnd ();

    glBegin(GL_QUADS);//right minar
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.15f, -0.20f);
    glVertex2f(0.25f, -0.20f);
    glVertex2f(0.25f, 0.55f);
    glVertex2f(0.15f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//right miner border-1
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.25f);
    glVertex2f(0.275f, 0.25f);
    glVertex2f(0.275f, 0.30f);
    glVertex2f(0.125f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);//right miner border -2
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.525f);
    glVertex2f(0.275f, 0.525f);
    glVertex2f(0.275f, 0.55f);
    glVertex2f(0.125f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.170f, 0.30f);
    glVertex2f(0.230f, 0.30f);
    glVertex2f(0.230f, 0.45f);
    glVertex2f(0.20f, 0.475f);
    glVertex2f(0.170f, 0.45f);
    glEnd();

    glPushMatrix();
    glTranslatef(position4,0.0f, 0.0f);
    // 1st sky
    int i;
    GLfloat p1 = 0.20f; GLfloat q1 = 0.95f;
    GLfloat r1 = 0.05f;
    int triangle1 = 40;
    GLfloat tp1 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p1,q1);
    for(i = 0; i <= triangle1; i++){
        glVertex2f(p1+(r1*cos(i*tp1/triangle1)), q1+(r1*sin(i*tp1/triangle1)));
    }
    glEnd();

    GLfloat p2 = 0.32f; GLfloat q2 = 0.95f;
    GLfloat r2 = 0.10f;
    int triangle2 = 40;
    GLfloat tp2 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p2,q2);
    for(i = 0; i <= triangle2; i++){
        glVertex2f(p2+(r2*cos(i*tp2/triangle2)), q2+(r2*sin(i*tp2/triangle2)));
    }
    glEnd();

    GLfloat p3 = 0.45f; GLfloat q3 = 0.95f;
    GLfloat r3 = 0.06f;
    int triangle3 = 40;
    GLfloat tp3 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p3,q3);
    for(i = 0; i <= triangle3; i++){
        glVertex2f(p3+(r3*cos(i*tp3/triangle3)), q3+(r3*sin(i*tp3/triangle3)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position5,0.0f, 0.0f);
    //2nd sky
    GLfloat p4 = 0.03f; GLfloat q4 = 0.77f;
    GLfloat r4 = 0.05f;
    int triangle4 = 40;
    GLfloat tp4 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p4,q4);
    for(i = 0; i <= triangle4; i++){
        glVertex2f(p4+(r4*cos(i*tp4/triangle4)), q4+(r4*sin(i*tp4/triangle4)));
    }
    glEnd();

    GLfloat p5 = 0.15f; GLfloat q5 = 0.77f;
    GLfloat r5 = 0.10f;
    int triangle5 = 40;
    GLfloat tp5 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p5,q5);
    for(i = 0; i <= triangle5; i++){
        glVertex2f(p5+(r5*cos(i*tp5/triangle5)), q5+(r5*sin(i*tp5/triangle5)));
    }
    glEnd();

    GLfloat p6 = 0.28f; GLfloat q6 = 0.77f;
    GLfloat r6 = 0.06f;
    int triangle6 = 40;
    GLfloat tp6 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p6,q6);
    for(i = 0; i <= triangle6; i++){
        glVertex2f(p6+(r6*cos(i*tp6/triangle6)), q6+(r6*sin(i*tp6/triangle6)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position6,0.0f, 0.0f);
    //3rd sky
    GLfloat p7 = -0.25f; GLfloat q7 = 0.90f;
    GLfloat r7 = 0.05f;
    int triangle7 = 40;
    GLfloat tp7 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p7,q7);
    for(i = 0; i <= triangle7; i++){
        glVertex2f(p7+(r7*cos(i*tp7/triangle7)), q7+(r7*sin(i*tp7/triangle7)));
    }
    glEnd();

    GLfloat p8 = -0.13f; GLfloat q8 = 0.90f;
    GLfloat r8 = 0.10f;
    int triangle8 = 40;
    GLfloat tp8 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p8,q8);
    for(i = 0; i <= triangle8; i++){
        glVertex2f(p8+(r8*cos(i*tp8/triangle8)), q8+(r8*sin(i*tp8/triangle8)));
    }
    glEnd();

    GLfloat p9 = 0.0f; GLfloat q9 = 0.90f;
    GLfloat r9 = 0.06f;
    int triangle9 = 40;
    GLfloat tp9 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p9,q9);
    for(i = 0; i <= triangle9; i++){
        glVertex2f(p9+(r9*cos(i*tp9/triangle9)), q9+(r9*sin(i*tp9/triangle9)));
    }
    glEnd();
     glPopMatrix();


    glPushMatrix();
    glTranslatef(position1,0.0f, 0.0f);
    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.30f, 0.75f);
    glVertex2f(-0.285f, 0.765f);
    glVertex2f(-0.30f, 0.75f);
    glVertex2f(-0.315f, 0.765f);
    glEnd();

    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.40f, 0.70f);
    glVertex2f(-0.385f, 0.715f);
    glVertex2f(-0.40f, 0.70f);
    glVertex2f(-0.415f, 0.715f);
    glEnd();

    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.33f, 0.68f);
    glVertex2f(-0.315f, 0.695f);
    glVertex2f(-0.33f, 0.68f);
    glVertex2f(-0.345f, 0.695f);
    glEnd();

    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.38f, 0.76f);
    glVertex2f(-0.365f, 0.775f);
    glVertex2f(-0.38f, 0.76f);
    glVertex2f(-0.395f, 0.775f);
    glEnd();

    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.27f, 0.72f);
    glVertex2f(-0.255f, 0.735f);
    glVertex2f(-0.27f, 0.72f);
    glVertex2f(-0.285f, 0.735f);
    glEnd();
     glPopMatrix();



   int a;//under stand circle

    GLfloat p100=0.63f; GLfloat q100= -0.83f; GLfloat r100 = 0.08f;
    int tringle100=40;

    GLfloat tp100 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p100,q100);
    for(a= 0;a<=tringle100; a++)
    {
        glVertex2f (
                    p100+(r100*cos(a*tp100/tringle100)),
                    q100+(r100*sin(a*tp100/tringle100))
                    );
    }
    glEnd ();



    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.425f, -0.83f);
    glVertex2f(0.435f, -0.83f);
    glVertex2f(0.435f, -0.65f);
    glVertex2f(0.425f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.39f, -0.78f);
    glVertex2f(0.40f, -0.78f);
    glVertex2f(0.40f, -0.65f);
    glVertex2f(0.39f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.46f, -0.78f);
    glVertex2f(0.47f, -0.78f);
    glVertex2f(0.47f, -0.65f);
    glVertex2f(0.46f, -0.65f);
    glEnd();

    GLfloat p30=0.43f; GLfloat q30= -0.63f; GLfloat r30 = 0.05f;
    int tringle30=40;

    GLfloat tp30 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p30,q30);
    for(a= 0;a<=tringle30; a++)
    {
        glVertex2f (p30+(r30*cos(a*tp30/tringle30)),q30+(r30*sin(a*tp30/tringle30)));
    }
    glEnd ();


    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.825f, -0.83f);
    glVertex2f(0.835f, -0.83f);
    glVertex2f(0.835f, -0.65f);
    glVertex2f(0.825f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.79f, -0.78f);
    glVertex2f(0.80f, -0.78f);
    glVertex2f(0.80f, -0.65f);
    glVertex2f(0.79f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.86f, -0.78f);
    glVertex2f(0.87f, -0.78f);
    glVertex2f(0.87f, -0.65f);
    glVertex2f(0.86f, -0.65f);
    glEnd();

    GLfloat p31=0.83f; GLfloat q31= -0.63f; GLfloat r31 = 0.05f;
    int tringle31=40;

    GLfloat tp31 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p31,q31);
    for(a= 0;a<=tringle31; a++)
    {
        glVertex2f (p31+(r31*cos(a*tp31/tringle31)),q31+(r31*sin(a*tp31/tringle31)));
    }
    glEnd ();


    glBegin(GL_QUADS);//CHAIR
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.35f, -0.15f);
    glVertex2f(0.50f, -0.15f);
    glVertex2f(0.525f, -0.05f);
    glVertex2f(0.375f, -0.05f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.36f, -0.15f);
    glVertex2f(0.36f, -0.225f);

    glVertex2f(0.49f, -0.15f);
    glVertex2f(0.49f, -0.225f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, -0.145f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, 0.05f);

    glVertex2f(0.38f, -0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.025f);
    glVertex2f(0.38f, 0.025f);

    glVertex2f(0.52f, 0.00f);
    glVertex2f(0.38f, 0.00f);

    glEnd();



    glBegin(GL_QUADS);//table stand
    glColor3ub(112,128,144);
    glVertex2f(0.62f, -0.81f);
    glVertex2f(0.64f, -0.81f);
    glColor3ub(119,136,153);
    glVertex2f(0.64f, -0.64f);
    glVertex2f(0.62f, -0.64f);
    glEnd();

         int j;//bigger circle of foyara
    GLfloat p11=0.63f; GLfloat q11= -0.52f; GLfloat r11 = 0.132f;
    int tringle11=40;

    GLfloat tp11 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3f ( 0.46f,0.46f,0.46f);//grey color
    glVertex2f (p11,q11);
    for(j= 0;j<=tringle11; j++)
    {
    glVertex2f (
    p11+(r11*cos(j*tp11/tringle11)),
    q11+(r11*sin(j*tp11/tringle11))
    );
    }
    glEnd ();



    int k;//small circle foyara
    GLfloat p12=0.63f; GLfloat q12= -0.50f; GLfloat r12 = 0.115f;
    int tringle12=40;

    GLfloat tp12 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub (188,143,143);//blue color
    glVertex2f (p12,q12);
    for(k= 0;k<=tringle12; k++)
    {
    glVertex2f (
    p12+(r12*cos(k*tp12/tringle12)),
    q12+(r12*sin(k*tp12/tringle12))
    );
    }
    glEnd ();


    //left mic
    GLfloat p13 = -0.75f; GLfloat q13 = 0.375f;
    GLfloat r13 = 0.04f;
    int triangle13 = 40;
    GLfloat tp13 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p13,q13);
    for(i = 0; i <= triangle13; i++){
        glVertex2f(p13+(r13*cos(i*tp13/triangle13)), q13+(r13*sin(i*tp13/triangle13)));
    }
    glEnd();

    GLfloat p14 = -0.75f; GLfloat q14 = 0.375f;
    GLfloat r14 = 0.01f;
    int triangle14 = 40;
    GLfloat tp14 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p14,q14);
    for(i = 0; i <= triangle14; i++){
        glVertex2f(p14+(r14*cos(i*tp14/triangle14)), q14+(r14*sin(i*tp14/triangle14)));
    }
    glEnd();

    //right mic
    GLfloat p15 = 0.2f; GLfloat q15 = 0.375f;
    GLfloat r15 = 0.04f;
    int triangle15 = 40;
    GLfloat tp15 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p15,q15);
    for(i = 0; i <= triangle15; i++){
        glVertex2f(p15+(r15*cos(i*tp15/triangle15)), q15+(r15*sin(i*tp15/triangle15)));
    }
    glEnd();

    GLfloat p16 = 0.2f; GLfloat q16 = 0.375f;
    GLfloat r16 = 0.01f;
    int triangle16 = 40;
    GLfloat tp16 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p16,q16);
    for(i = 0; i <= triangle16; i++){
        glVertex2f(p16+(r16*cos(i*tp16/triangle16)), q16+(r16*sin(i*tp16/triangle16)));
    }
    glEnd();

   glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.30f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.28f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.28f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.32f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.32f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.29f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.095f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.105f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.325f, 0.25f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.305f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.23f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.345f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.27f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.24f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.350f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.33f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.28f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.370f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.32f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.29f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.90f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.88f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.28f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.92f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.32f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.29f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.095f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.105f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.925f, 0.25f);

    //flower
    glColor3ub(235, 235, 111);
    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.905f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.23f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.945f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.27f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.24f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.950f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.93f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.28f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.970f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.32f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.29f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.29f);

    glEnd();


    int l;//left minar gombuj

    GLfloat p17=-0.751f; GLfloat q17= 0.55f; GLfloat r17= 0.07f;
    int tringle17=40;

    GLfloat tp17 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (p17,q17);
    for(l= 0;l<=tringle17; l++)
    {
    glVertex2f (
    p17+(r17*cos(l*tp17/tringle17)),
    q17+(r17*sin(l*tp17/tringle17))
    );
    }
    glEnd ();

    int s;// Right minar gombuj

    GLfloat x18=0.2f; GLfloat h18= 0.55f; GLfloat Gr18 = 0.07f;
    int Gtringle18=40;

    GLfloat tp18 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x18,h18);
    for(s= 0;s<=Gtringle18; s++)
    {
    glVertex2f (
    x18+(Gr18*cos(s*tp18/Gtringle18)),
    h18+(Gr18*sin(s*tp18/Gtringle18))
    );
    }
    glEnd ();

    //2nd left gombuj
    GLfloat x1=-0.604f; GLfloat h1= 0.515f; GLfloat ra = 0.07f;
    int Gtringle=40;

    GLfloat tp19 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x1,h1);
    for(l= 0;l<=Gtringle; l++)
    {
    glVertex2f (
    x1+(ra*cos(l*tp19/Gtringle)),
    h1+(ra*sin(l*tp19/Gtringle))
    );
    }
    glEnd ();

    int m;// 3rd left gombuj

    GLfloat x2=-0.460f; GLfloat h2= 0.515f; GLfloat Gr = 0.07f;
    int Gtringle1=40;

    GLfloat tp20 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x2,h2);
    for(m= 0;m<=Gtringle1; m++)
    {
    glVertex2f (
    x2+(Gr*cos(m*tp20/Gtringle1)),
    h2+(Gr*sin(m*tp20/Gtringle1))
    );
    }
    glEnd ();

    int n;// 4th left gombuj

    GLfloat x3=-0.315f; GLfloat h3= 0.515f; GLfloat Gr1 = 0.07f;
    int Gtringle21=40;

    GLfloat tp21 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x3,h3);
    for(n= 0;n<=Gtringle21; n++)
    {
    glVertex2f (
        x3+(Gr1*cos(n*tp21/Gtringle21)),
        h3+(Gr1*sin(n*tp21/Gtringle21))
    );
    }
    glEnd ();

    int p;// 5th left gombuj

    GLfloat x4=-0.170f; GLfloat h4= 0.515f; GLfloat Gr2 = 0.07f;
    int Gtringle22=40;

    GLfloat tp22=1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x4,h4);
    for(p= 0;p<=Gtringle22; p++)
    {
    glVertex2f (
    x4+(Gr2*cos(p*tp22/Gtringle22)),
    h4+(Gr2*sin(p*tp22/Gtringle22))
    );
    }
    glEnd ();

    int q;// 6th left gombuj

    GLfloat x5=-0.028f; GLfloat h5= 0.515f; GLfloat Gr3 = 0.07f;
    int Gtringle23=40;

    GLfloat tp23 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x5,h5);
    for(q= 0;q<=Gtringle23; q++)
    {
    glVertex2f (
    x5+(Gr3*cos(q*tp23/Gtringle23)),
    h5+(Gr3*sin(q*tp23/Gtringle23))
    );
    }
    glEnd ();

    glBegin(GL_TRIANGLES);//right side  small tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.00f, -0.35f);//middle line
    glVertex2f(0.10f, -0.35f);
    glVertex2f(0.05f, -0.15f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.05f, -0.25f);//middle line
    glVertex2f(0.10f, -0.4f);
    glVertex2f(0.00f, -0.4f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.05f, -0.30f);//middle line
    glVertex2f(0.10f, -0.45f);
    glVertex2f(0.00f, -0.45f);

    glEnd();



    glBegin(GL_TRIANGLES);//left side  small tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.55f, -0.35f);//middle line
    glVertex2f(-0.65f, -0.35f);
    glVertex2f(-0.60f, -0.15f);

    glEnd();
        glBegin(GL_TRIANGLES);//left side  small tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.55f, -0.4f);//middle line
    glVertex2f(-0.65f, -0.4f);
    glVertex2f(-0.60f, -0.25f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  small tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.55f, -0.45f);//middle line
    glVertex2f(-0.65f, -0.45f);
    glVertex2f(-0.60f, -0.30f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side  middle tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.15f, -0.40f);//middle line
    glVertex2f(0.1f, -0.6f);
    glVertex2f(0.2f, -0.6f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.15f, -0.50f);//middle line
    glVertex2f(0.1f, -0.65f);
    glVertex2f(0.2f, -0.65f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.15f, -0.55f);//middle line
    glVertex2f(0.1f, -0.7f);
    glVertex2f(0.2f, -0.7f);
    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.70f, -0.40f);//middle line
    glVertex2f(-0.75f, -0.60f);
    glVertex2f(-0.65f, -0.60f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.70f, -0.50f);//middle line
    glVertex2f(-0.75f, -0.65f);
    glVertex2f(-0.65f, -0.65f);

    glEnd();


    glBegin(GL_TRIANGLES);//left side  middle tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.70f, -0.55f);//middle line
    glVertex2f(-0.75f, -0.70f);
    glVertex2f(-0.65f, -0.70f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side last tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.25f, -0.65f);//middle line
    glVertex2f(0.2f, -0.85f);
    glVertex2f(0.3f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side last tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.25f, -0.75f);//middle line
    glVertex2f(0.2f, -0.90f);
    glVertex2f(0.3f, -0.90f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  last tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.25f, -0.80f);//middle line
    glVertex2f(0.2f, -0.95f);
    glVertex2f(0.3f, -0.95f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.80f, -0.65f);//middle line
    glVertex2f(-0.85f, -0.85f);
    glVertex2f(-0.75f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  last tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.80f, -0.75f);//middle line
    glVertex2f(-0.85f, -0.9f);
    glVertex2f(-0.75f, -0.9f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.80f, -0.8f);//middle line
    glVertex2f(-0.85f, -0.95f);
    glVertex2f(-0.75f, -0.95f);

    glEnd();




GLfloat p26=-0.95f; GLfloat q26= 0.7f; GLfloat r26 = 0.15f;// big tree
int tringle26=40;
GLfloat tp26 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub ( 0,130,05);
glVertex2f (p26,q26);
for(i= 0;i<=tringle26; i++)
{
glVertex2f(p26+(r26*cos(i*tp26/tringle26)),q26+(r26*sin(i*tp26/tringle26)));
}
glEnd ();

//int j; //Right Leaf
GLfloat p27=-0.75f; GLfloat q27= 0.7f; GLfloat r27 = 0.15f;
int tringle27=40;
GLfloat tp27 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub ( 0,110,0);
glVertex2f (p27,q27);
for(i= 0;i<=tringle27; i++)
{
glVertex2f (p27+(r27*cos(i*tp27/tringle27)),q27+(r27*sin(i*tp27/tringle27)));
}
glEnd ();




//int k; //Upper Leaf
GLfloat p28=-0.85f; GLfloat q28= 0.85f; GLfloat r28 = 0.15f;
int tringle28=40;
GLfloat tp28 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub ( 0,100,0);
glVertex2f (p28,q28);
for(i= 0;i<=tringle28; i++)
{
glVertex2f (
p28+(r28*cos(i*tp28/tringle28)),q28+(r28*sin(i*tp28/tringle28)));
}
glEnd ();


glBegin(GL_QUADS);//Tree ground
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.85f, -0.2f);
glVertex2f(-0.95f, -0.2f);
glEnd();



glBegin(GL_QUADS); //Left Tributariness
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-1.0f, 0.6f);
glVertex2f(-0.95f, 0.6f);
glVertex2f(-0.85f, 0.5f);
glEnd();


glBegin(GL_QUADS); //Right Tributariness
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.95f, 0.5f);
glEnd();


glBegin(GL_POLYGON); //Right Tributariness
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.82f, 0.8f);
glVertex2f(-0.79f, 0.8f);
glVertex2f(-0.8f, 0.6f);
glEnd();


glBegin(GL_QUADS);
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();


    glBegin(GL_QUADS);
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();

/*glBegin(GL_POLYGON);// TREE BOTTOM SURFACE
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-1.0f, -0.15f);
glVertex2f(-1.0f, -0.3f);
glVertex2f(-0.95f, -0.25f);
glVertex2f(-0.95f, -0.20f);
glVertex2f(-0.85f, -0.20f);
glVertex2f(-0.85f, -0.25f);
glVertex2f(-0.80f, -0.30f);
glVertex2f(-0.80f, -0.15f);
glEnd();*/

glBegin(GL_QUADS);//road
glColor3f(1.0f, 0.6f, 0.09f);
//glColor3f(0.58f, 0.23f, 0.19f);
glVertex2f(-0.47f, -0.25f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(0.15f, -1.0f);
glVertex2f(-0.65f, -1.0f);

glEnd();

glBegin(GL_LINES);//road
glColor3f(0.87f, 0.68f, 0.45f);
glVertex2f(-0.475f, -0.30f);
glVertex2f(-0.0875f, -0.30f);
glVertex2f(-0.490f, -0.35f);
glVertex2f(-0.07f, -0.35f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.515f, -0.45f);
glVertex2f(-0.035f, -0.45f);
glVertex2f(-0.525f, -0.5f);
glVertex2f(-0.02f, -0.5f);
glVertex2f(-0.54f, -0.55f);
glVertex2f(0.00f, -0.55f);
glVertex2f(-0.554f, -0.6f);
glVertex2f(0.017f, -0.6f);
glVertex2f(-0.562f, -0.65f);
glVertex2f(0.0355f, -0.65f);
glVertex2f(-0.575f, -0.7f);
glVertex2f(0.052f, -0.7f);
glVertex2f(-0.587f, -0.75f);
glVertex2f(0.064f, -0.75f);
glVertex2f(-0.6f, -0.8f);
glVertex2f(0.084f, -0.8f);
glVertex2f(-0.615f, -0.85f);
glVertex2f(0.1f, -0.85f);
glVertex2f(-0.625f, -0.9f);
glVertex2f(0.12f, -0.9f);
glVertex2f(-0.64f, -0.95f);
glVertex2f(0.135f, -0.95f);
glVertex2f(-0.60f, -0.8f);
glVertex2f(-0.60f, -1.0f);
glVertex2f(-0.55f, -0.6f);
glVertex2f(-0.55f, -1.0f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.5f, -1.0f);
glVertex2f(-0.45f, -0.25f);
glVertex2f(-0.45f, -1.0f);
glVertex2f(-0.4f, -0.25f);
glVertex2f(-0.4f, -1.0f);
glVertex2f(-0.35f, -0.25f);
glVertex2f(-0.35f, -1.0f);
glVertex2f(-0.3f, -0.25f);
glVertex2f(-0.3f, -1.0f);
glVertex2f(-0.25f, -0.25f);
glVertex2f(-0.25f, -1.0f);
glVertex2f(-0.2f, -0.25f);
glVertex2f(-0.2f, -1.0f);
glVertex2f(-0.15f, -0.25f);
glVertex2f(-0.15f, -1.0f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(-0.1f, -1.0f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.05f, -1.0f);
glVertex2f(0.0f, -0.55f);
glVertex2f(0.0f, -1.0f);
glVertex2f(0.05f, -0.7f);
glVertex2f(0.05f, -1.0f);
glVertex2f(0.10f, -0.85f);
glVertex2f(0.10f, -1.0f);
glEnd();



    glBegin(GL_QUADS);//ghatpar
    glColor3f(0.21f, 0.16f, 0.10f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.85f, 0.15f);
    glVertex2f(0.75f, 0.15f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.71f, 0.0f);
    glVertex2f(0.71f, 0.08f);
    glVertex2f(0.70f, 0.08f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.72f, 0.060f);
    glVertex2f(0.73f, 0.060f);
    glVertex2f(0.73f, 0.12f);
    glVertex2f(0.72f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.74f, 0.12f);
    glVertex2f(0.75f, 0.12f);
    glVertex2f(0.75f, 0.18f);
    glVertex2f(0.74f, 0.18f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.890f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.9f, 0.08f);
    glVertex2f(0.890f, 0.08f);
    glEnd();

    glBegin(GL_QUADS);//ghatpar right middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.87f, 0.060f);
    glVertex2f(0.88f, 0.060f);
    glVertex2f(0.88f, 0.12f);
    glVertex2f(0.87f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.85f, 0.12f);
    glVertex2f(0.86f, 0.12f);
    glVertex2f(0.86f, 0.18f);
    glVertex2f(0.85f, 0.18f);
    glEnd();




    glFlush();
}




























// night mood


    void night()
    { glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glBegin(GL_QUADS);//GROUND
    glColor3ub(27,77,62);
    glVertex2f(-1.0f, -1.0f);
    glVertex2f(1.0f, -1.0f);
    glVertex2f(1.0f, 0.55f);
    glVertex2f(-1.0f, 0.55f);
    glEnd();

    glBegin(GL_QUADS);// SKY
    glColor3f(.28f, .31f, .42f);
    glVertex2f(-1.0f, 0.55f);
    glColor3f(.28f, .31f, .42f);
    glVertex2f(1.0f, 0.55f);
    glColor3f(.10f,.10f,.13f);
    glVertex2f(1.0f, 1.0f);
    glColor3f(.10f,.10f,.13f);
    glVertex2f(-1.0f, 1.0f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(51,112,93);
    glVertex2f(-0.50f, 0.55f);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.40f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(51,112,93);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(-0.20f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(51,112,93);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(0.10f, 0.55f);
    glVertex2f(0.0f, 0.65f);
    glEnd();


    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(8, 115, 10);
    glVertex2f(-1.0f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.90f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.70f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.50f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(-0.30f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(-0.10f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(0.10f, 0.65f);
    glEnd();


    //glPushMatrix();
    //glTranslatef(0.0f, position2, 0.0f);
    int x;//moon
    GLfloat p10 = -0.60f; GLfloat q10 = 0.90f;
    GLfloat r10 = 0.085f;
    int triangle10 = 40;
    GLfloat tp10 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(245, 243, 242);
    //glColor3ub(255, 255, 0);
    glVertex2f(p10,q10);
    for(x = 0; x <= triangle10; x++){
        glVertex2f(p10+(r10*cos(x*tp10/triangle10)), q10+(r10*sin(x*tp10/triangle10)));
    }

    glEnd();
       // glPopMatrix();



    glBegin(GL_POLYGON);//river
    //glColor3ub(86, 98, 133);
        glColor3ub(47,85, 138);

    //glColor3f(0.40f, 0.71f, 1.0f);
    glVertex2f(-1.0f, 0.10f);
   // glColor3ub(86, 98, 133);

    glVertex2f(1.0f, 0.10f);

    //glColor3ub(40, 45, 61);
    //glColor3f(.72f, .93f, .122f);
    glVertex2f(1.0f, 0.55f);
        //glColor3f(.72f, .93f, .122f);

    //glColor3ub(40, 45, 61);
    glVertex2f(-1.0f, 0.55f);
    glEnd();


    glPushMatrix();
    glTranslatef(position,0.0f, 0.0f);
    glBegin(GL_QUADS);// streamer
    glColor3f(0.36f, 0.20f, 0.09f);
    glVertex2f(0.70f, 0.125f);
    glVertex2f(0.90f, 0.125f);
    glVertex2f(0.95f, 0.175f);
    glVertex2f(0.65f, 0.175f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.45f, 0.26f);
    glVertex2f(0.70f, 0.175f);
    glVertex2f(0.90f, 0.175f);
    glVertex2f(0.90f, 0.225f);
    glVertex2f(0.70f, 0.225f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.725f, 0.1875f);
    glVertex2f(0.75f, 0.1875f);
    glVertex2f(0.75f, 0.2125f);
    glVertex2f(0.725f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.775f, 0.1875f);
    glVertex2f(0.80f, 0.1875f);
    glVertex2f(0.80f, 0.2125f);
    glVertex2f(0.775f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.825f, 0.1875f);
    glVertex2f(0.85f, 0.1875f);
    glVertex2f(0.85f, 0.2125f);
    glVertex2f(0.825f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.875f, 0.1875f);
    glVertex2f(0.90f, 0.1875f);
    glVertex2f(0.90f, 0.2125f);
    glVertex2f(0.875f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.725f, 0.225f);
    glVertex2f(0.875f, 0.225f);
    glVertex2f(0.875f, 0.255f);
    glVertex2f(0.725f, 0.255f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.745f, 0.255f);
    glVertex2f(0.760f, 0.255f);
    glVertex2f(0.760f, 0.305f);
    glVertex2f(0.745f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.775f, 0.255f);
    glVertex2f(0.79f, 0.255f);
    glVertex2f(0.79f, 0.305f);
    glVertex2f(0.775f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.805f, 0.255f);
    glVertex2f(0.82f, 0.255f);
    glVertex2f(0.82f, 0.305f);
    glVertex2f(0.805f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.835f, 0.255f);
    glVertex2f(0.85f, 0.255f);
    glVertex2f(0.85f, 0.305f);
    glVertex2f(0.835f, 0.305f);
    glEnd();

    glPopMatrix();



    glPushMatrix();
    glTranslatef(position3,0.0f, 0.0f);
    glBegin(GL_QUADS);//boat
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.47f, 0.325f);
    glVertex2f(0.65f, 0.325f);
    glVertex2f(0.70f, 0.375f);
    glVertex2f(0.42f, 0.375f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.49f, 0.375f);
    glVertex2f(0.49f, 0.475f);

    glVertex2f(0.50f, 0.375f);
    glVertex2f(0.50f, 0.525f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.55f, 0.375f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.57f, 0.375f);

    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.455f, 0.38f);
    glVertex2f(0.49f, 0.38f);
    glVertex2f(0.49f, 0.475f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.50f, 0.38f);
    glVertex2f(0.535f, 0.38f);
    glVertex2f(0.50f, 0.575f);
    glEnd();

    glBegin(GL_TRIANGLES);//maji
    glColor3ub(21, 176, 34);
    glVertex2f(0.63f, 0.40f);
    glVertex2f(0.67f, 0.40f);
    glVertex2f(0.65f, 0.45f);
    glEnd();

    int v;//maji

    GLfloat p24=0.65f; GLfloat q24= 0.45f; GLfloat r24 = 0.01f;
    int Gtringle24=40;

    GLfloat tp24 =2.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(207, 202, 155);
    glVertex2f (p24,q24);
    for(v= 0;v<=Gtringle24; v++)
    {
    glVertex2f (
    p24+(r24*cos(v*tp24/Gtringle24)),
    q24+(r24*sin(v*tp24/Gtringle24))
    );
    }
    glEnd ();

    glBegin(GL_LINES);//maji
    glColor3ub(207, 202, 155);
    glVertex2f(0.645f, 0.40f);
    glVertex2f(0.645f, 0.375f);

    glVertex2f(0.655f, 0.40f);
    glVertex2f(0.655f, 0.375f);

    glVertex2f(0.66f, 0.425f);
    glVertex2f(0.62f, 0.415f);

    glVertex2f(0.64f, 0.425f);
    glVertex2f(0.61f, 0.435f);

    glColor3ub(145, 86, 3);
    glVertex2f(0.61f, 0.445f);
    glVertex2f(0.66f, 0.30f);

    glEnd();
        glPopMatrix();




    glBegin(GL_QUADS);//Mosque
     glColor3ub(168, 102, 50);
    glVertex2f(-0.75f, -0.20f);
    glVertex2f(0.20f, -0.20f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(-0.75f, 0.50f);
    glEnd();

    glBegin(GL_QUADS);//Mosque upper border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.75f, 0.50f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(0.20f, 0.515f);
    glVertex2f(-0.75f, 0.515f);
    glEnd();

    glBegin(GL_QUADS);//Road
    glColor3f(0.97f, 0.98f, 1.0f);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.45f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow left
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(-0.65f, -1.0f);
    glVertex2f(-0.45f, -0.20f);
    glVertex2f(-0.47f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow right
    glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.12f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Mosque bottom border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, -0.25f);
    glVertex2f(0.275f, -0.25f);
    glVertex2f(0.275f, -0.20f);
    glVertex2f(-0.825f, -0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.35f, -0.20f);
    glVertex2f(-0.20f, -0.20f);
    glVertex2f(-0.20f, 0.20f);
    glVertex2f(-0.225f, 0.30f);
    glVertex2f(-0.275f, 0.35f);
    glVertex2f(-0.325f, 0.30f);
    glVertex2f(-0.35f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.325f, -0.20f);
    glVertex2f(-0.21f, -0.20f);
    glVertex2f(-0.21f, 0.20f);
    glVertex2f(-0.228f, 0.28f);
    glVertex2f(-0.275f, 0.32f);
    glVertex2f(-0.31f, 0.28f);
    glVertex2f(-0.325f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.15f, -0.20f);
    glVertex2f(-0.05f, -0.20f);
    glVertex2f(-0.05f, 0.15f);
    glVertex2f(-0.075f, 0.2125f);
    glVertex2f(-0.10f, 0.25f);
    glVertex2f(-0.125f, 0.2125f);
    glVertex2f(-0.15f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.135f, -0.20f);
    glVertex2f(-0.058f, -0.20f);
    glVertex2f(-0.058f, 0.15f);
    glVertex2f(-0.085f, 0.2125f);
    glVertex2f(-0.10f, 0.225f);
    glVertex2f(-0.112f, 0.2125f);
    glVertex2f(-0.135f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.0f, -0.20f);
    glVertex2f(0.10f, -0.20f);
    glVertex2f(0.10f, 0.15f);
    glVertex2f(0.075f, 0.2125f);
    glVertex2f(0.050f, 0.25f);
    glVertex2f(0.025f, 0.2125f);
    glVertex2f(0.0f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.015f, -0.20f);
    glVertex2f(0.092f, -0.20f);
    glVertex2f(0.092f, 0.15f);
    glVertex2f(0.065f, 0.2125f);
    glVertex2f(0.050f, 0.225f);
    glVertex2f(0.037f, 0.2125f);
    glVertex2f(0.015f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.50f, -0.20f);
    glVertex2f(-0.40f, -0.20f);
    glVertex2f(-0.40f, 0.15f);
    glVertex2f(-0.425f, 0.2125f);
    glVertex2f(-0.45f, 0.25f);
    glVertex2f(-0.475f, 0.2125f);
    glVertex2f(-0.50f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.492f, -0.20f);
    glVertex2f(-0.415f, -0.20f);
    glVertex2f(-0.4155f, 0.15f);
    glVertex2f(-0.437f, 0.2125f);
    glVertex2f(-0.45f, 0.225f);
    glVertex2f(-0.465f, 0.2125f);
    glVertex2f(-0.492f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.65f, -0.20f);
    glVertex2f(-0.55f, -0.20f);
    glVertex2f(-0.55f, 0.15f);
    glVertex2f(-0.575f, 0.2125f);
    glVertex2f(-0.60f, 0.25f);
    glVertex2f(-0.625f, 0.2125f);
    glVertex2f(-0.65f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.642f, -0.20f);
    glVertex2f(-0.565f, -0.20f);
    glVertex2f(-0.565f, 0.15f);
    glVertex2f(-0.587f, 0.2125f);
    glVertex2f(-0.60f, 0.225f);
    glVertex2f(-0.615f, 0.2125f);
    glVertex2f(-0.642f, 0.15f);
    glEnd();


    glBegin(GL_QUADS);//left minar
    glColor3ub(143, 81, 34);
    glVertex2f(-0.80f, -0.20f);
    glVertex2f(-0.70f, -0.20f);
    glVertex2f(-0.70f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//left miner border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.25f);
    glVertex2f(-0.675f, 0.25f);
    glVertex2f(-0.675f, 0.30f);
    glVertex2f(-0.825f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.525f);
    glVertex2f(-0.675f, 0.525f);
    glVertex2f(-0.675f, 0.55f);
    glVertex2f(-0.825f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.78f, 0.30f);
    glVertex2f(-0.72f, 0.30f);
    glVertex2f(-0.72f, 0.45f);
    glVertex2f(-0.75f, 0.475f);
    glVertex2f(-0.78f, 0.45f);
    glEnd();

    int t;// Right 2nd gombuj

    GLfloat x7=0.11f; GLfloat h7= 0.515f; GLfloat Gr5 = 0.07f;
    int Gtringle25=40;

    GLfloat tp25 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x7,h7);
    for(t= 0;t<=Gtringle25; t++)
    {
    glVertex2f (
    x7+(Gr5*cos(t*tp25/Gtringle25)),
    h7+(Gr5*sin(t*tp25/Gtringle25))
    );
    }
    glEnd ();

    glBegin(GL_QUADS);//right minar
     glColor3ub(143, 81, 34);
    glVertex2f(0.15f, -0.20f);
    glVertex2f(0.25f, -0.20f);
    glVertex2f(0.25f, 0.55f);
    glVertex2f(0.15f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//right miner border-1
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.25f);
    glVertex2f(0.275f, 0.25f);
    glVertex2f(0.275f, 0.30f);
    glVertex2f(0.125f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);//right miner border -2
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.525f);
    glVertex2f(0.275f, 0.525f);
    glVertex2f(0.275f, 0.55f);
    glVertex2f(0.125f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.170f, 0.30f);
    glVertex2f(0.230f, 0.30f);
    glVertex2f(0.230f, 0.45f);
    glVertex2f(0.20f, 0.475f);
    glVertex2f(0.170f, 0.45f);
    glEnd();

    glPushMatrix();
    glTranslatef(position4,0.0f, 0.0f);
    // 1st sky
    int i;
    GLfloat p1 = 0.20f; GLfloat q1 = 0.95f;
    GLfloat r1 = 0.05f;
    int triangle1 = 40;
    GLfloat tp1 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p1,q1);
    for(i = 0; i <= triangle1; i++){
        glVertex2f(p1+(r1*cos(i*tp1/triangle1)), q1+(r1*sin(i*tp1/triangle1)));
    }
    glEnd();

    GLfloat p2 = 0.32f; GLfloat q2 = 0.95f;
    GLfloat r2 = 0.10f;
    int triangle2 = 40;
    GLfloat tp2 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p2,q2);
    for(i = 0; i <= triangle2; i++){
        glVertex2f(p2+(r2*cos(i*tp2/triangle2)), q2+(r2*sin(i*tp2/triangle2)));
    }
    glEnd();

    GLfloat p3 = 0.45f; GLfloat q3 = 0.95f;
    GLfloat r3 = 0.06f;
    int triangle3 = 40;
    GLfloat tp3 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p3,q3);
    for(i = 0; i <= triangle3; i++){
        glVertex2f(p3+(r3*cos(i*tp3/triangle3)), q3+(r3*sin(i*tp3/triangle3)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position5,0.0f, 0.0f);
    //2nd sky
    GLfloat p4 = 0.03f; GLfloat q4 = 0.77f;
    GLfloat r4 = 0.05f;
    int triangle4 = 40;
    GLfloat tp4 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p4,q4);
    for(i = 0; i <= triangle4; i++){
        glVertex2f(p4+(r4*cos(i*tp4/triangle4)), q4+(r4*sin(i*tp4/triangle4)));
    }
    glEnd();

    GLfloat p5 = 0.15f; GLfloat q5 = 0.77f;
    GLfloat r5 = 0.10f;
    int triangle5 = 40;
    GLfloat tp5 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p5,q5);
    for(i = 0; i <= triangle5; i++){
        glVertex2f(p5+(r5*cos(i*tp5/triangle5)), q5+(r5*sin(i*tp5/triangle5)));
    }
    glEnd();

    GLfloat p6 = 0.28f; GLfloat q6 = 0.77f;
    GLfloat r6 = 0.06f;
    int triangle6 = 40;
    GLfloat tp6 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p6,q6);
    for(i = 0; i <= triangle6; i++){
        glVertex2f(p6+(r6*cos(i*tp6/triangle6)), q6+(r6*sin(i*tp6/triangle6)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position6,0.0f, 0.0f);
    //3rd sky
    GLfloat p7 = -0.25f; GLfloat q7 = 0.90f;
    GLfloat r7 = 0.05f;
    int triangle7 = 40;
    GLfloat tp7 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p7,q7);
    for(i = 0; i <= triangle7; i++){
        glVertex2f(p7+(r7*cos(i*tp7/triangle7)), q7+(r7*sin(i*tp7/triangle7)));
    }
    glEnd();

    GLfloat p8 = -0.13f; GLfloat q8 = 0.90f;
    GLfloat r8 = 0.10f;
    int triangle8 = 40;
    GLfloat tp8 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p8,q8);
    for(i = 0; i <= triangle8; i++){
        glVertex2f(p8+(r8*cos(i*tp8/triangle8)), q8+(r8*sin(i*tp8/triangle8)));
    }
    glEnd();

    GLfloat p9 = 0.0f; GLfloat q9 = 0.90f;
    GLfloat r9 = 0.06f;
    int triangle9 = 40;
    GLfloat tp9 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p9,q9);
    for(i = 0; i <= triangle9; i++){
        glVertex2f(p9+(r9*cos(i*tp9/triangle9)), q9+(r9*sin(i*tp9/triangle9)));
    }
    glEnd();
     glPopMatrix();



   int a;//under stand circle

    GLfloat p100=0.63f; GLfloat q100= -0.83f; GLfloat r100 = 0.08f;
    int tringle100=40;

    GLfloat tp100 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p100,q100);
    for(a= 0;a<=tringle100; a++)
    {
        glVertex2f (
                    p100+(r100*cos(a*tp100/tringle100)),
                    q100+(r100*sin(a*tp100/tringle100))
                    );

    }
    glEnd ();



    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.425f, -0.83f);
    glVertex2f(0.435f, -0.83f);
    glVertex2f(0.435f, -0.65f);
    glVertex2f(0.425f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.39f, -0.78f);
    glVertex2f(0.40f, -0.78f);
    glVertex2f(0.40f, -0.65f);
    glVertex2f(0.39f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.46f, -0.78f);
    glVertex2f(0.47f, -0.78f);
    glVertex2f(0.47f, -0.65f);
    glVertex2f(0.46f, -0.65f);
    glEnd();

    GLfloat p30=0.43f; GLfloat q30= -0.63f; GLfloat r30 = 0.05f;
    int tringle30=40;

    GLfloat tp30 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p30,q30);
    for(a= 0;a<=tringle30; a++)
    {
        glVertex2f (p30+(r30*cos(a*tp30/tringle30)),q30+(r30*sin(a*tp30/tringle30)));
    }
    glEnd ();


    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.825f, -0.83f);
    glVertex2f(0.835f, -0.83f);
    glVertex2f(0.835f, -0.65f);
    glVertex2f(0.825f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.79f, -0.78f);
    glVertex2f(0.80f, -0.78f);
    glVertex2f(0.80f, -0.65f);
    glVertex2f(0.79f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.86f, -0.78f);
    glVertex2f(0.87f, -0.78f);
    glVertex2f(0.87f, -0.65f);
    glVertex2f(0.86f, -0.65f);
    glEnd();

    GLfloat p31=0.83f; GLfloat q31= -0.63f; GLfloat r31 = 0.05f;
    int tringle31=40;

    GLfloat tp31 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p31,q31);
    for(a= 0;a<=tringle31; a++)
    {
        glVertex2f (p31+(r31*cos(a*tp31/tringle31)),q31+(r31*sin(a*tp31/tringle31)));
    }
    glEnd ();


    glBegin(GL_QUADS);//CHAIR
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.35f, -0.15f);
    glVertex2f(0.50f, -0.15f);
    glVertex2f(0.525f, -0.05f);
    glVertex2f(0.375f, -0.05f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.36f, -0.15f);
    glVertex2f(0.36f, -0.225f);

    glVertex2f(0.49f, -0.15f);
    glVertex2f(0.49f, -0.225f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, -0.145f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, 0.05f);

    glVertex2f(0.38f, -0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.025f);
    glVertex2f(0.38f, 0.025f);

    glVertex2f(0.52f, 0.00f);
    glVertex2f(0.38f, 0.00f);

    glEnd();



    glBegin(GL_QUADS);//table stand
    glColor3ub(112,128,144);
    glVertex2f(0.62f, -0.81f);
    glVertex2f(0.64f, -0.81f);
    glColor3ub(119,136,153);
    glVertex2f(0.64f, -0.64f);
    glVertex2f(0.62f, -0.64f);
    glEnd();

         int j;//bigger circle of foyara
    GLfloat p11=0.63f; GLfloat q11= -0.52f; GLfloat r11 = 0.132f;
    int tringle11=40;

    GLfloat tp11 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3f ( 0.46f,0.46f,0.46f);//grey color
    glVertex2f (p11,q11);
    for(j= 0;j<=tringle11; j++)
    {
    glVertex2f (
    p11+(r11*cos(j*tp11/tringle11)),
    q11+(r11*sin(j*tp11/tringle11))
    );
    }
    glEnd ();



    int k;//small circle foyara
    GLfloat p12=0.63f; GLfloat q12= -0.50f; GLfloat r12 = 0.115f;
    int tringle12=40;

    GLfloat tp12 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub (188,143,143);//blue color
    glVertex2f (p12,q12);
    for(k= 0;k<=tringle12; k++)
    {
    glVertex2f (
    p12+(r12*cos(k*tp12/tringle12)),
    q12+(r12*sin(k*tp12/tringle12))
    );
    }
    glEnd ();


    //left mic
    GLfloat p13 = -0.75f; GLfloat q13 = 0.375f;
    GLfloat r13 = 0.04f;
    int triangle13 = 40;
    GLfloat tp13 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p13,q13);
    for(i = 0; i <= triangle13; i++){
        glVertex2f(p13+(r13*cos(i*tp13/triangle13)), q13+(r13*sin(i*tp13/triangle13)));
    }
    glEnd();

    GLfloat p14 = -0.75f; GLfloat q14 = 0.375f;
    GLfloat r14 = 0.01f;
    int triangle14 = 40;
    GLfloat tp14 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p14,q14);
    for(i = 0; i <= triangle14; i++){
        glVertex2f(p14+(r14*cos(i*tp14/triangle14)), q14+(r14*sin(i*tp14/triangle14)));
    }
    glEnd();

    //right mic
    GLfloat p15 = 0.2f; GLfloat q15 = 0.375f;
    GLfloat r15 = 0.04f;
    int triangle15 = 40;
    GLfloat tp15 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p15,q15);
    for(i = 0; i <= triangle15; i++){
        glVertex2f(p15+(r15*cos(i*tp15/triangle15)), q15+(r15*sin(i*tp15/triangle15)));
    }
    glEnd();

    GLfloat p16 = 0.2f; GLfloat q16 = 0.375f;
    GLfloat r16 = 0.01f;
    int triangle16 = 40;
    GLfloat tp16 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p16,q16);
    for(i = 0; i <= triangle16; i++){
        glVertex2f(p16+(r16*cos(i*tp16/triangle16)), q16+(r16*sin(i*tp16/triangle16)));
    }
    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.30f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.28f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.28f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.32f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.32f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.29f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.095f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.105f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.325f, 0.25f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.305f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.23f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.345f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.27f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.24f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.350f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.33f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.28f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.370f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.32f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.29f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.90f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.88f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.28f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.92f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.32f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.29f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.095f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.105f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.925f, 0.25f);

    //flower
    glColor3ub(235, 235, 111);
    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.905f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.23f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.945f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.27f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.24f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.950f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.93f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.28f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.970f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.32f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.29f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.29f);

    glEnd();


    int l;//left minar gombuj

    GLfloat p17=-0.751f; GLfloat q17= 0.55f; GLfloat r17= 0.07f;
    int tringle17=40;

    GLfloat tp17 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (p17,q17);
    for(l= 0;l<=tringle17; l++)
    {
    glVertex2f (
    p17+(r17*cos(l*tp17/tringle17)),
    q17+(r17*sin(l*tp17/tringle17))
    );
    }
    glEnd ();

    int s;// Right minar gombuj

    GLfloat x18=0.2f; GLfloat h18= 0.55f; GLfloat Gr18 = 0.07f;
    int Gtringle18=40;

    GLfloat tp18 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x18,h18);
    for(s= 0;s<=Gtringle18; s++)
    {
    glVertex2f (
    x18+(Gr18*cos(s*tp18/Gtringle18)),
    h18+(Gr18*sin(s*tp18/Gtringle18))
    );
    }
    glEnd ();

    //2nd left gombuj
    GLfloat x1=-0.604f; GLfloat h1= 0.515f; GLfloat ra = 0.07f;
    int Gtringle=40;

    GLfloat tp19 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x1,h1);
    for(l= 0;l<=Gtringle; l++)
    {
    glVertex2f (
    x1+(ra*cos(l*tp19/Gtringle)),
    h1+(ra*sin(l*tp19/Gtringle))
    );
    }
    glEnd ();

    int m;// 3rd left gombuj

    GLfloat x2=-0.460f; GLfloat h2= 0.515f; GLfloat Gr = 0.07f;
    int Gtringle1=40;

    GLfloat tp20 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x2,h2);
    for(m= 0;m<=Gtringle1; m++)
    {
    glVertex2f (
    x2+(Gr*cos(m*tp20/Gtringle1)),
    h2+(Gr*sin(m*tp20/Gtringle1))
    );
    }
    glEnd ();

    int n;// 4th left gombuj

    GLfloat x3=-0.315f; GLfloat h3= 0.515f; GLfloat Gr1 = 0.07f;
    int Gtringle21=40;

    GLfloat tp21 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x3,h3);
    for(n= 0;n<=Gtringle21; n++)
    {
    glVertex2f (
        x3+(Gr1*cos(n*tp21/Gtringle21)),
        h3+(Gr1*sin(n*tp21/Gtringle21))
    );
    }
    glEnd ();

    int p;// 5th left gombuj

    GLfloat x4=-0.170f; GLfloat h4= 0.515f; GLfloat Gr2 = 0.07f;
    int Gtringle22=40;

    GLfloat tp22=1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x4,h4);
    for(p= 0;p<=Gtringle22; p++)
    {
    glVertex2f (
    x4+(Gr2*cos(p*tp22/Gtringle22)),
    h4+(Gr2*sin(p*tp22/Gtringle22))
    );
    }
    glEnd ();

    int q;// 6th left gombuj

    GLfloat x5=-0.028f; GLfloat h5= 0.515f; GLfloat Gr3 = 0.07f;
    int Gtringle23=40;

    GLfloat tp23 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x5,h5);
    for(q= 0;q<=Gtringle23; q++)
    {
    glVertex2f (
    x5+(Gr3*cos(q*tp23/Gtringle23)),
    h5+(Gr3*sin(q*tp23/Gtringle23))
    );
    }
    glEnd ();

    glBegin(GL_TRIANGLES);//right side  small tree triangle-1
   glColor3ub(47, 92, 13);
    glVertex2f(0.00f, -0.35f);//middle line
    glVertex2f(0.10f, -0.35f);
    glVertex2f(0.05f, -0.15f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-2
   glColor3ub(47, 92, 13);
    glVertex2f(0.05f, -0.25f);//middle line
    glVertex2f(0.10f, -0.4f);
    glVertex2f(0.00f, -0.4f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-3
   glColor3ub(47, 92, 13);
    glVertex2f(0.05f, -0.30f);//middle line
    glVertex2f(0.10f, -0.45f);
    glVertex2f(0.00f, -0.45f);

    glEnd();



    glBegin(GL_TRIANGLES);//left side  small tree triangle-1
    glColor3ub(47, 92, 13);
    glVertex2f(-0.55f, -0.35f);//middle line
    glVertex2f(-0.65f, -0.35f);
    glVertex2f(-0.60f, -0.15f);

    glEnd();
        glBegin(GL_TRIANGLES);//left side  small tree triangle-2
 glColor3ub(47, 92, 13);
    glVertex2f(-0.55f, -0.4f);//middle line
    glVertex2f(-0.65f, -0.4f);
    glVertex2f(-0.60f, -0.25f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  small tree triangle-3
glColor3ub(47, 92, 13);
    glVertex2f(-0.55f, -0.45f);//middle line
    glVertex2f(-0.65f, -0.45f);
    glVertex2f(-0.60f, -0.30f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side  middle tree triangle-1
glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -0.40f);//middle line
    glVertex2f(0.1f, -0.6f);
    glVertex2f(0.2f, -0.6f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-2
    glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -0.50f);//middle line
    glVertex2f(0.1f, -0.65f);
    glVertex2f(0.2f, -0.65f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-3
glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -0.55f);//middle line
    glVertex2f(0.1f, -0.7f);
    glVertex2f(0.2f, -0.7f);
    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -0.40f);//middle line
    glVertex2f(-0.75f, -0.60f);
    glVertex2f(-0.65f, -0.60f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -0.50f);//middle line
    glVertex2f(-0.75f, -0.65f);
    glVertex2f(-0.65f, -0.65f);

    glEnd();


    glBegin(GL_TRIANGLES);//left side  middle tree triangle-2
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -0.55f);//middle line
    glVertex2f(-0.75f, -0.70f);
    glVertex2f(-0.65f, -0.70f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side last tree triangle-1
   glColor3ub(47, 92, 13);
    glVertex2f(0.25f, -0.65f);//middle line
    glVertex2f(0.2f, -0.85f);
    glVertex2f(0.3f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side last tree triangle-2
    glColor3ub(47, 92, 13);
    glVertex2f(0.25f, -0.75f);//middle line
    glVertex2f(0.2f, -0.90f);
    glVertex2f(0.3f, -0.90f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  last tree triangle-3
    glColor3ub(47, 92, 13);
    glVertex2f(0.25f, -0.80f);//middle line
    glVertex2f(0.2f, -0.95f);
    glVertex2f(0.3f, -0.95f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-1
   glColor3ub(47, 92, 13);
    glVertex2f(-0.80f, -0.65f);//middle line
    glVertex2f(-0.85f, -0.85f);
    glVertex2f(-0.75f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  last tree triangle-2
   glColor3ub(47, 92, 13);
    glVertex2f(-0.80f, -0.75f);//middle line
    glVertex2f(-0.85f, -0.9f);
    glVertex2f(-0.75f, -0.9f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-3
    glColor3ub(47, 92, 13);
    glVertex2f(-0.80f, -0.8f);//middle line
    glVertex2f(-0.85f, -0.95f);
    glVertex2f(-0.75f, -0.95f);

    glEnd();




GLfloat p26=-0.95f; GLfloat q26= 0.7f; GLfloat r26 = 0.15f;// big tree
int tringle26=40;
GLfloat tp26 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub(47, 92, 13);
glVertex2f (p26,q26);
for(i= 0;i<=tringle26; i++)
{
glVertex2f(p26+(r26*cos(i*tp26/tringle26)),q26+(r26*sin(i*tp26/tringle26)));
}
glEnd ();


//int j; //Right Leaf
GLfloat p27=-0.75f; GLfloat q27= 0.7f; GLfloat r27 = 0.15f;
int tringle27=40;
GLfloat tp27 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub(47, 92, 13);
glVertex2f (p27,q27);
for(i= 0;i<=tringle27; i++)
{
glVertex2f (p27+(r27*cos(i*tp27/tringle27)),q27+(r27*sin(i*tp27/tringle27)));
}
glEnd ();


//int k; //Upper Leaf
GLfloat p28=-0.85f; GLfloat q28= 0.85f; GLfloat r28 = 0.15f;
int tringle28=40;
GLfloat tp28 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub(47, 92, 13);
glVertex2f (p28,q28);
for(i= 0;i<=tringle28; i++)
{
glVertex2f (
p28+(r28*cos(i*tp28/tringle28)),q28+(r28*sin(i*tp28/tringle28)));
}
glEnd ();



glBegin(GL_QUADS);//Tree ground
glColor3ub(58,24,16);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.85f, -0.2f);
glVertex2f(-0.95f, -0.2f);
glEnd();


glBegin(GL_QUADS); //Left Tributariness
glColor3ub(58,24,16);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-1.0f, 0.6f);
glVertex2f(-0.95f, 0.6f);
glVertex2f(-0.85f, 0.5f);
glEnd();


glBegin(GL_QUADS); //Right Tributariness
glColor3ub(58,24,16);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.95f, 0.5f);
glEnd();


glBegin(GL_POLYGON); //Right Tributariness
glColor3ub(58,24,16);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.82f, 0.8f);
glVertex2f(-0.79f, 0.8f);
glVertex2f(-0.8f, 0.6f);
glEnd();


glBegin(GL_QUADS);
glColor3ub(58,24,16);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();


    glBegin(GL_QUADS);
glColor3ub(58,24,16);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();

/*glBegin(GL_POLYGON);// TREE BOTTOM SURFACE
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-1.0f, -0.15f);
glVertex2f(-1.0f, -0.3f);
glVertex2f(-0.95f, -0.25f);
glVertex2f(-0.95f, -0.20f);
glVertex2f(-0.85f, -0.20f);
glVertex2f(-0.85f, -0.25f);
glVertex2f(-0.80f, -0.30f);
glVertex2f(-0.80f, -0.15f);
glEnd();*/

glBegin(GL_QUADS);//road
glColor3ub(64,33,5);
//glColor3f(0.58f, 0.23f, 0.19f);
glVertex2f(-0.47f, -0.25f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(0.15f, -1.0f);
glVertex2f(-0.65f, -1.0f);

glEnd();

glBegin(GL_LINES);//road
glColor3ub(89,38,11);
glVertex2f(-0.475f, -0.30f);
glVertex2f(-0.0875f, -0.30f);
glVertex2f(-0.490f, -0.35f);
glVertex2f(-0.07f, -0.35f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.515f, -0.45f);
glVertex2f(-0.035f, -0.45f);
glVertex2f(-0.525f, -0.5f);
glVertex2f(-0.02f, -0.5f);
glVertex2f(-0.54f, -0.55f);
glVertex2f(0.00f, -0.55f);
glVertex2f(-0.554f, -0.6f);
glVertex2f(0.017f, -0.6f);
glVertex2f(-0.562f, -0.65f);
glVertex2f(0.0355f, -0.65f);
glVertex2f(-0.575f, -0.7f);
glVertex2f(0.052f, -0.7f);
glVertex2f(-0.587f, -0.75f);
glVertex2f(0.064f, -0.75f);
glVertex2f(-0.6f, -0.8f);
glVertex2f(0.084f, -0.8f);
glVertex2f(-0.615f, -0.85f);
glVertex2f(0.1f, -0.85f);
glVertex2f(-0.625f, -0.9f);
glVertex2f(0.12f, -0.9f);
glVertex2f(-0.64f, -0.95f);
glVertex2f(0.135f, -0.95f);
glVertex2f(-0.60f, -0.8f);
glVertex2f(-0.60f, -1.0f);
glVertex2f(-0.55f, -0.6f);
glVertex2f(-0.55f, -1.0f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.5f, -1.0f);
glVertex2f(-0.45f, -0.25f);
glVertex2f(-0.45f, -1.0f);
glVertex2f(-0.4f, -0.25f);
glVertex2f(-0.4f, -1.0f);
glVertex2f(-0.35f, -0.25f);
glVertex2f(-0.35f, -1.0f);
glVertex2f(-0.3f, -0.25f);
glVertex2f(-0.3f, -1.0f);
glVertex2f(-0.25f, -0.25f);
glVertex2f(-0.25f, -1.0f);
glVertex2f(-0.2f, -0.25f);
glVertex2f(-0.2f, -1.0f);
glVertex2f(-0.15f, -0.25f);
glVertex2f(-0.15f, -1.0f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(-0.1f, -1.0f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.05f, -1.0f);
glVertex2f(0.0f, -0.55f);
glVertex2f(0.0f, -1.0f);
glVertex2f(0.05f, -0.7f);
glVertex2f(0.05f, -1.0f);
glVertex2f(0.10f, -0.85f);
glVertex2f(0.10f, -1.0f);
glEnd();



    glBegin(GL_QUADS);//ghatpar
    glColor3f(0.21f, 0.16f, 0.10f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.85f, 0.15f);
    glVertex2f(0.75f, 0.15f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.71f, 0.0f);
    glVertex2f(0.71f, 0.08f);
    glVertex2f(0.70f, 0.08f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.72f, 0.060f);
    glVertex2f(0.73f, 0.060f);
    glVertex2f(0.73f, 0.12f);
    glVertex2f(0.72f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.74f, 0.12f);
    glVertex2f(0.75f, 0.12f);
    glVertex2f(0.75f, 0.18f);
    glVertex2f(0.74f, 0.18f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.890f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.9f, 0.08f);
    glVertex2f(0.890f, 0.08f);
    glEnd();

    glBegin(GL_QUADS);//ghatpar right middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.87f, 0.060f);
    glVertex2f(0.88f, 0.060f);
    glVertex2f(0.88f, 0.12f);
    glVertex2f(0.87f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.85f, 0.12f);
    glVertex2f(0.86f, 0.12f);
    glVertex2f(0.86f, 0.18f);
    glVertex2f(0.85f, 0.18f);
    glEnd();


  glFlush();
}

















//fajr mood

void fajr()
    { glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glBegin(GL_QUADS);//GROUND
    glColor3ub(27,77,62);
    glVertex2f(-1.0f, -1.0f);
    glVertex2f(1.0f, -1.0f);
    glVertex2f(1.0f, 0.55f);
    glVertex2f(-1.0f, 0.55f);
    glEnd();

    glBegin(GL_QUADS);// SKY
    glColor3ub(39, 100, 150);
    glVertex2f(-1.0f, 0.55f);
    //glColor3ub(.28f, .31f, .42f);
    glVertex2f(1.0f, 0.55f);
    glColor3ub(32, 80, 120);
    glVertex2f(1.0f, 1.0f);
    //glColor3f(.10f,.10f,.13f);
    glVertex2f(-1.0f, 1.0f);
    glEnd();

        int x;//moon
    GLfloat p10 = -0.60f; GLfloat q10 = 0.56f;
    GLfloat r10 = 0.085f;
    int triangle10 = 40;
    GLfloat tp10 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(245, 243, 242);
    //glColor3ub(255, 255, 0);
    glVertex2f(p10,q10);
    for(x = 0; x <= triangle10; x++){
        glVertex2f(p10+(r10*cos(x*tp10/triangle10)), q10+(r10*sin(x*tp10/triangle10)));
    }

    glEnd();
    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(51,112,93);
    glVertex2f(-0.50f, 0.55f);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.40f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(51,112,93);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(-0.20f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(51,112,93);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(0.10f, 0.55f);
    glVertex2f(0.0f, 0.65f);
    glEnd();


    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(8, 115, 10);
    glVertex2f(-1.0f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.90f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.70f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.50f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(-0.30f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(-0.10f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(0.10f, 0.65f);
    glEnd();



    //glPushMatrix();
    //glTranslatef(0.0f, position2, 0.0f);

        //glPopMatrix();



    glBegin(GL_POLYGON);//river
    //glColor3ub(86, 98, 133);
      glColor3ub(38, 100, 150);
        glColor3ub(22, 63, 94);

    //glColor3f(0.40f, 0.71f, 1.0f);
    glVertex2f(-1.0f, 0.10f);
   // glColor3ub(86, 98, 133);

    glVertex2f(1.0f, 0.10f);
   glColor3ub(22, 63, 94);
   // glColor3ub(38, 100, 150);
    //glColor3f(.72f, .93f, .122f);
    glVertex2f(1.0f, 0.55f);
        //glColor3f(.72f, .93f, .122f);

    //glColor3ub(40, 45, 61);
    glVertex2f(-1.0f, 0.55f);
    glEnd();



    glPushMatrix();
    glTranslatef(position,0.0f, 0.0f);
    glBegin(GL_QUADS);// streamer
    glColor3f(0.36f, 0.20f, 0.09f);
    glVertex2f(0.70f, 0.125f);
    glVertex2f(0.90f, 0.125f);
    glVertex2f(0.95f, 0.175f);
    glVertex2f(0.65f, 0.175f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.45f, 0.26f);
    glVertex2f(0.70f, 0.175f);
    glVertex2f(0.90f, 0.175f);
    glVertex2f(0.90f, 0.225f);
    glVertex2f(0.70f, 0.225f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.725f, 0.1875f);
    glVertex2f(0.75f, 0.1875f);
    glVertex2f(0.75f, 0.2125f);
    glVertex2f(0.725f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.775f, 0.1875f);
    glVertex2f(0.80f, 0.1875f);
    glVertex2f(0.80f, 0.2125f);
    glVertex2f(0.775f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.825f, 0.1875f);
    glVertex2f(0.85f, 0.1875f);
    glVertex2f(0.85f, 0.2125f);
    glVertex2f(0.825f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.875f, 0.1875f);
    glVertex2f(0.90f, 0.1875f);
    glVertex2f(0.90f, 0.2125f);
    glVertex2f(0.875f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.725f, 0.225f);
    glVertex2f(0.875f, 0.225f);
    glVertex2f(0.875f, 0.255f);
    glVertex2f(0.725f, 0.255f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.745f, 0.255f);
    glVertex2f(0.760f, 0.255f);
    glVertex2f(0.760f, 0.305f);
    glVertex2f(0.745f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.775f, 0.255f);
    glVertex2f(0.79f, 0.255f);
    glVertex2f(0.79f, 0.305f);
    glVertex2f(0.775f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.805f, 0.255f);
    glVertex2f(0.82f, 0.255f);
    glVertex2f(0.82f, 0.305f);
    glVertex2f(0.805f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.835f, 0.255f);
    glVertex2f(0.85f, 0.255f);
    glVertex2f(0.85f, 0.305f);
    glVertex2f(0.835f, 0.305f);
    glEnd();

    glPopMatrix();



    glPushMatrix();
    glTranslatef(position3,0.0f, 0.0f);
    glBegin(GL_QUADS);//boat
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.47f, 0.325f);
    glVertex2f(0.65f, 0.325f);
    glVertex2f(0.70f, 0.375f);
    glVertex2f(0.42f, 0.375f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.49f, 0.375f);
    glVertex2f(0.49f, 0.475f);

    glVertex2f(0.50f, 0.375f);
    glVertex2f(0.50f, 0.525f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.55f, 0.375f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.57f, 0.375f);

    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.455f, 0.38f);
    glVertex2f(0.49f, 0.38f);
    glVertex2f(0.49f, 0.475f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.50f, 0.38f);
    glVertex2f(0.535f, 0.38f);
    glVertex2f(0.50f, 0.575f);
    glEnd();

    glBegin(GL_TRIANGLES);//maji
    glColor3ub(21, 176, 34);
    glVertex2f(0.63f, 0.40f);
    glVertex2f(0.67f, 0.40f);
    glVertex2f(0.65f, 0.45f);
    glEnd();

    int v;//maji

    GLfloat p24=0.65f; GLfloat q24= 0.45f; GLfloat r24 = 0.01f;
    int Gtringle24=40;

    GLfloat tp24 =2.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(207, 202, 155);
    glVertex2f (p24,q24);
    for(v= 0;v<=Gtringle24; v++)
    {
    glVertex2f (
    p24+(r24*cos(v*tp24/Gtringle24)),
    q24+(r24*sin(v*tp24/Gtringle24))
    );
    }
    glEnd ();

    glBegin(GL_LINES);//maji
    glColor3ub(207, 202, 155);
    glVertex2f(0.645f, 0.40f);
    glVertex2f(0.645f, 0.375f);

    glVertex2f(0.655f, 0.40f);
    glVertex2f(0.655f, 0.375f);

    glVertex2f(0.66f, 0.425f);
    glVertex2f(0.62f, 0.415f);

    glVertex2f(0.64f, 0.425f);
    glVertex2f(0.61f, 0.435f);

    glColor3ub(145, 86, 3);
    glVertex2f(0.61f, 0.445f);
    glVertex2f(0.66f, 0.30f);

    glEnd();
        glPopMatrix();




    glBegin(GL_QUADS);//Mosque
    glColor3ub(168, 102, 50);
    glVertex2f(-0.75f, -0.20f);
    glVertex2f(0.20f, -0.20f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(-0.75f, 0.50f);
    glEnd();

    glBegin(GL_QUADS);//Mosque upper border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.75f, 0.50f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(0.20f, 0.515f);
    glVertex2f(-0.75f, 0.515f);
    glEnd();

    glBegin(GL_QUADS);//Road
    glColor3f(0.97f, 0.98f, 1.0f);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.45f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow left
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(-0.65f, -1.0f);
    glVertex2f(-0.45f, -0.20f);
    glVertex2f(-0.47f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow right
    glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.12f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Mosque bottom border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, -0.25f);
    glVertex2f(0.275f, -0.25f);
    glVertex2f(0.275f, -0.20f);
    glVertex2f(-0.825f, -0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.35f, -0.20f);
    glVertex2f(-0.20f, -0.20f);
    glVertex2f(-0.20f, 0.20f);
    glVertex2f(-0.225f, 0.30f);
    glVertex2f(-0.275f, 0.35f);
    glVertex2f(-0.325f, 0.30f);
    glVertex2f(-0.35f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.325f, -0.20f);
    glVertex2f(-0.21f, -0.20f);
    glVertex2f(-0.21f, 0.20f);
    glVertex2f(-0.228f, 0.28f);
    glVertex2f(-0.275f, 0.32f);
    glVertex2f(-0.31f, 0.28f);
    glVertex2f(-0.325f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.15f, -0.20f);
    glVertex2f(-0.05f, -0.20f);
    glVertex2f(-0.05f, 0.15f);
    glVertex2f(-0.075f, 0.2125f);
    glVertex2f(-0.10f, 0.25f);
    glVertex2f(-0.125f, 0.2125f);
    glVertex2f(-0.15f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.135f, -0.20f);
    glVertex2f(-0.058f, -0.20f);
    glVertex2f(-0.058f, 0.15f);
    glVertex2f(-0.085f, 0.2125f);
    glVertex2f(-0.10f, 0.225f);
    glVertex2f(-0.112f, 0.2125f);
    glVertex2f(-0.135f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.0f, -0.20f);
    glVertex2f(0.10f, -0.20f);
    glVertex2f(0.10f, 0.15f);
    glVertex2f(0.075f, 0.2125f);
    glVertex2f(0.050f, 0.25f);
    glVertex2f(0.025f, 0.2125f);
    glVertex2f(0.0f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.015f, -0.20f);
    glVertex2f(0.092f, -0.20f);
    glVertex2f(0.092f, 0.15f);
    glVertex2f(0.065f, 0.2125f);
    glVertex2f(0.050f, 0.225f);
    glVertex2f(0.037f, 0.2125f);
    glVertex2f(0.015f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.50f, -0.20f);
    glVertex2f(-0.40f, -0.20f);
    glVertex2f(-0.40f, 0.15f);
    glVertex2f(-0.425f, 0.2125f);
    glVertex2f(-0.45f, 0.25f);
    glVertex2f(-0.475f, 0.2125f);
    glVertex2f(-0.50f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.492f, -0.20f);
    glVertex2f(-0.415f, -0.20f);
    glVertex2f(-0.4155f, 0.15f);
    glVertex2f(-0.437f, 0.2125f);
    glVertex2f(-0.45f, 0.225f);
    glVertex2f(-0.465f, 0.2125f);
    glVertex2f(-0.492f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.65f, -0.20f);
    glVertex2f(-0.55f, -0.20f);
    glVertex2f(-0.55f, 0.15f);
    glVertex2f(-0.575f, 0.2125f);
    glVertex2f(-0.60f, 0.25f);
    glVertex2f(-0.625f, 0.2125f);
    glVertex2f(-0.65f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.642f, -0.20f);
    glVertex2f(-0.565f, -0.20f);
    glVertex2f(-0.565f, 0.15f);
    glVertex2f(-0.587f, 0.2125f);
    glVertex2f(-0.60f, 0.225f);
    glVertex2f(-0.615f, 0.2125f);
    glVertex2f(-0.642f, 0.15f);
    glEnd();


    glBegin(GL_QUADS);//left minar
    glColor3ub( 143, 81, 34);
    glVertex2f(-0.80f, -0.20f);
    glVertex2f(-0.70f, -0.20f);
    glVertex2f(-0.70f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//left miner border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.25f);
    glVertex2f(-0.675f, 0.25f);
    glVertex2f(-0.675f, 0.30f);
    glVertex2f(-0.825f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.525f);
    glVertex2f(-0.675f, 0.525f);
    glVertex2f(-0.675f, 0.55f);
    glVertex2f(-0.825f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.78f, 0.30f);
    glVertex2f(-0.72f, 0.30f);
    glVertex2f(-0.72f, 0.45f);
    glVertex2f(-0.75f, 0.475f);
    glVertex2f(-0.78f, 0.45f);
    glEnd();

    int t;// Right 2nd gombuj

    GLfloat x7=0.11f; GLfloat h7= 0.515f; GLfloat Gr5 = 0.07f;
    int Gtringle25=40;

    GLfloat tp25 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x7,h7);
    for(t= 0;t<=Gtringle25; t++)
    {
    glVertex2f (
    x7+(Gr5*cos(t*tp25/Gtringle25)),
    h7+(Gr5*sin(t*tp25/Gtringle25))
    );
    }
    glEnd ();

    glBegin(GL_QUADS);//right minar
    glColor3ub( 143, 81, 34);
    glVertex2f(0.15f, -0.20f);
    glVertex2f(0.25f, -0.20f);
    glVertex2f(0.25f, 0.55f);
    glVertex2f(0.15f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//right miner border-1
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.25f);
    glVertex2f(0.275f, 0.25f);
    glVertex2f(0.275f, 0.30f);
    glVertex2f(0.125f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);//right miner border -2
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.525f);
    glVertex2f(0.275f, 0.525f);
    glVertex2f(0.275f, 0.55f);
    glVertex2f(0.125f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.170f, 0.30f);
    glVertex2f(0.230f, 0.30f);
    glVertex2f(0.230f, 0.45f);
    glVertex2f(0.20f, 0.475f);
    glVertex2f(0.170f, 0.45f);
    glEnd();

    glPushMatrix();
    glTranslatef(position4,0.0f, 0.0f);
    // 1st sky
    int i;
    GLfloat p1 = 0.20f; GLfloat q1 = 0.95f;
    GLfloat r1 = 0.05f;
    int triangle1 = 40;
    GLfloat tp1 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p1,q1);
    for(i = 0; i <= triangle1; i++){
        glVertex2f(p1+(r1*cos(i*tp1/triangle1)), q1+(r1*sin(i*tp1/triangle1)));
    }
    glEnd();

    GLfloat p2 = 0.32f; GLfloat q2 = 0.95f;
    GLfloat r2 = 0.10f;
    int triangle2 = 40;
    GLfloat tp2 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p2,q2);
    for(i = 0; i <= triangle2; i++){
        glVertex2f(p2+(r2*cos(i*tp2/triangle2)), q2+(r2*sin(i*tp2/triangle2)));
    }
    glEnd();

    GLfloat p3 = 0.45f; GLfloat q3 = 0.95f;
    GLfloat r3 = 0.06f;
    int triangle3 = 40;
    GLfloat tp3 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p3,q3);
    for(i = 0; i <= triangle3; i++){
        glVertex2f(p3+(r3*cos(i*tp3/triangle3)), q3+(r3*sin(i*tp3/triangle3)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position5,0.0f, 0.0f);
    //2nd sky
    GLfloat p4 = 0.03f; GLfloat q4 = 0.77f;
    GLfloat r4 = 0.05f;
    int triangle4 = 40;
    GLfloat tp4 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p4,q4);
    for(i = 0; i <= triangle4; i++){
        glVertex2f(p4+(r4*cos(i*tp4/triangle4)), q4+(r4*sin(i*tp4/triangle4)));
    }
    glEnd();

    GLfloat p5 = 0.15f; GLfloat q5 = 0.77f;
    GLfloat r5 = 0.10f;
    int triangle5 = 40;
    GLfloat tp5 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p5,q5);
    for(i = 0; i <= triangle5; i++){
        glVertex2f(p5+(r5*cos(i*tp5/triangle5)), q5+(r5*sin(i*tp5/triangle5)));
    }
    glEnd();

    GLfloat p6 = 0.28f; GLfloat q6 = 0.77f;
    GLfloat r6 = 0.06f;
    int triangle6 = 40;
    GLfloat tp6 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p6,q6);
    for(i = 0; i <= triangle6; i++){
        glVertex2f(p6+(r6*cos(i*tp6/triangle6)), q6+(r6*sin(i*tp6/triangle6)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position6,0.0f, 0.0f);
    //3rd sky
    GLfloat p7 = -0.25f; GLfloat q7 = 0.90f;
    GLfloat r7 = 0.05f;
    int triangle7 = 40;
    GLfloat tp7 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p7,q7);
    for(i = 0; i <= triangle7; i++){
        glVertex2f(p7+(r7*cos(i*tp7/triangle7)), q7+(r7*sin(i*tp7/triangle7)));
    }
    glEnd();

    GLfloat p8 = -0.13f; GLfloat q8 = 0.90f;
    GLfloat r8 = 0.10f;
    int triangle8 = 40;
    GLfloat tp8 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p8,q8);
    for(i = 0; i <= triangle8; i++){
        glVertex2f(p8+(r8*cos(i*tp8/triangle8)), q8+(r8*sin(i*tp8/triangle8)));
    }
    glEnd();

    GLfloat p9 = 0.0f; GLfloat q9 = 0.90f;
    GLfloat r9 = 0.06f;
    int triangle9 = 40;
    GLfloat tp9 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(81,90,122);
    glVertex2f(p9,q9);
    for(i = 0; i <= triangle9; i++){
        glVertex2f(p9+(r9*cos(i*tp9/triangle9)), q9+(r9*sin(i*tp9/triangle9)));
    }
    glEnd();
     glPopMatrix();



   int a;//under stand circle

    GLfloat p100=0.63f; GLfloat q100= -0.83f; GLfloat r100 = 0.08f;
    int tringle100=40;

    GLfloat tp100 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p100,q100);
    for(a= 0;a<=tringle100; a++)
    {
        glVertex2f (
                    p100+(r100*cos(a*tp100/tringle100)),
                    q100+(r100*sin(a*tp100/tringle100))
                    );


    }
    glEnd ();


    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.425f, -0.83f);
    glVertex2f(0.435f, -0.83f);
    glVertex2f(0.435f, -0.65f);
    glVertex2f(0.425f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.39f, -0.78f);
    glVertex2f(0.40f, -0.78f);
    glVertex2f(0.40f, -0.65f);
    glVertex2f(0.39f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.46f, -0.78f);
    glVertex2f(0.47f, -0.78f);
    glVertex2f(0.47f, -0.65f);
    glVertex2f(0.46f, -0.65f);
    glEnd();

    GLfloat p30=0.43f; GLfloat q30= -0.63f; GLfloat r30 = 0.05f;
    int tringle30=40;

    GLfloat tp30 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p30,q30);
    for(a= 0;a<=tringle30; a++)
    {
        glVertex2f (p30+(r30*cos(a*tp30/tringle30)),q30+(r30*sin(a*tp30/tringle30)));
    }
    glEnd ();


    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.825f, -0.83f);
    glVertex2f(0.835f, -0.83f);
    glVertex2f(0.835f, -0.65f);
    glVertex2f(0.825f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.79f, -0.78f);
    glVertex2f(0.80f, -0.78f);
    glVertex2f(0.80f, -0.65f);
    glVertex2f(0.79f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.86f, -0.78f);
    glVertex2f(0.87f, -0.78f);
    glVertex2f(0.87f, -0.65f);
    glVertex2f(0.86f, -0.65f);
    glEnd();

    GLfloat p31=0.83f; GLfloat q31= -0.63f; GLfloat r31 = 0.05f;
    int tringle31=40;

    GLfloat tp31 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p31,q31);
    for(a= 0;a<=tringle31; a++)
    {
        glVertex2f (p31+(r31*cos(a*tp31/tringle31)),q31+(r31*sin(a*tp31/tringle31)));
    }
    glEnd ();


    glBegin(GL_QUADS);//CHAIR
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.35f, -0.15f);
    glVertex2f(0.50f, -0.15f);
    glVertex2f(0.525f, -0.05f);
    glVertex2f(0.375f, -0.05f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.36f, -0.15f);
    glVertex2f(0.36f, -0.225f);

    glVertex2f(0.49f, -0.15f);
    glVertex2f(0.49f, -0.225f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, -0.145f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, 0.05f);

    glVertex2f(0.38f, -0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.025f);
    glVertex2f(0.38f, 0.025f);

    glVertex2f(0.52f, 0.00f);
    glVertex2f(0.38f, 0.00f);

    glEnd();


    glBegin(GL_QUADS);//table stand
    glColor3ub(112,128,144);
    glVertex2f(0.62f, -0.81f);
    glVertex2f(0.64f, -0.81f);
    glColor3ub(119,136,153);
    glVertex2f(0.64f, -0.64f);
    glVertex2f(0.62f, -0.64f);
    glEnd();

         int j;//bigger circle of foyara
    GLfloat p11=0.63f; GLfloat q11= -0.52f; GLfloat r11 = 0.132f;
    int tringle11=40;

    GLfloat tp11 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3f ( 0.46f,0.46f,0.46f);//grey color
    glVertex2f (p11,q11);
    for(j= 0;j<=tringle11; j++)
    {
    glVertex2f (
    p11+(r11*cos(j*tp11/tringle11)),
    q11+(r11*sin(j*tp11/tringle11))
    );
    }
    glEnd ();



    int k;//small circle foyara
    GLfloat p12=0.63f; GLfloat q12= -0.50f; GLfloat r12 = 0.115f;
    int tringle12=40;

    GLfloat tp12 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub (188,143,143);//blue color
    glVertex2f (p12,q12);
    for(k= 0;k<=tringle12; k++)
    {
    glVertex2f (
    p12+(r12*cos(k*tp12/tringle12)),
    q12+(r12*sin(k*tp12/tringle12))
    );
    }
    glEnd ();


    //left mic
    GLfloat p13 = -0.75f; GLfloat q13 = 0.375f;
    GLfloat r13 = 0.04f;
    int triangle13 = 40;
    GLfloat tp13 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p13,q13);
    for(i = 0; i <= triangle13; i++){
        glVertex2f(p13+(r13*cos(i*tp13/triangle13)), q13+(r13*sin(i*tp13/triangle13)));
    }
    glEnd();

    GLfloat p14 = -0.75f; GLfloat q14 = 0.375f;
    GLfloat r14 = 0.01f;
    int triangle14 = 40;
    GLfloat tp14 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p14,q14);
    for(i = 0; i <= triangle14; i++){
        glVertex2f(p14+(r14*cos(i*tp14/triangle14)), q14+(r14*sin(i*tp14/triangle14)));
    }
    glEnd();

    //right mic
    GLfloat p15 = 0.2f; GLfloat q15 = 0.375f;
    GLfloat r15 = 0.04f;
    int triangle15 = 40;
    GLfloat tp15 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p15,q15);
    for(i = 0; i <= triangle15; i++){
        glVertex2f(p15+(r15*cos(i*tp15/triangle15)), q15+(r15*sin(i*tp15/triangle15)));
    }
    glEnd();

    GLfloat p16 = 0.2f; GLfloat q16 = 0.375f;
    GLfloat r16 = 0.01f;
    int triangle16 = 40;
    GLfloat tp16 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p16,q16);
    for(i = 0; i <= triangle16; i++){
        glVertex2f(p16+(r16*cos(i*tp16/triangle16)), q16+(r16*sin(i*tp16/triangle16)));
    }
    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.30f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.28f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.28f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.32f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.32f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.29f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.095f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.105f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.325f, 0.25f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.305f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.23f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.345f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.27f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.24f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.350f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.33f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.28f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.370f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.32f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.29f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.90f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.88f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.28f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.92f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.32f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.29f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.095f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.105f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.925f, 0.25f);

    //flower
    glColor3ub(235, 235, 111);
    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.905f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.23f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.945f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.27f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.24f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.950f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.93f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.28f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.970f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.32f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.29f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.29f);

    glEnd();


    int l;//left minar gombuj

    GLfloat p17=-0.751f; GLfloat q17= 0.55f; GLfloat r17= 0.07f;
    int tringle17=40;

    GLfloat tp17 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (p17,q17);
    for(l= 0;l<=tringle17; l++)
    {
    glVertex2f (
    p17+(r17*cos(l*tp17/tringle17)),
    q17+(r17*sin(l*tp17/tringle17))
    );
    }
    glEnd ();

    int s;// Right minar gombuj

    GLfloat x18=0.2f; GLfloat h18= 0.55f; GLfloat Gr18 = 0.07f;
    int Gtringle18=40;

    GLfloat tp18 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x18,h18);
    for(s= 0;s<=Gtringle18; s++)
    {
    glVertex2f (
    x18+(Gr18*cos(s*tp18/Gtringle18)),
    h18+(Gr18*sin(s*tp18/Gtringle18))
    );
    }
    glEnd ();

    //2nd left gombuj
    GLfloat x1=-0.604f; GLfloat h1= 0.515f; GLfloat ra = 0.07f;
    int Gtringle=40;

    GLfloat tp19 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x1,h1);
    for(l= 0;l<=Gtringle; l++)
    {
    glVertex2f (
    x1+(ra*cos(l*tp19/Gtringle)),
    h1+(ra*sin(l*tp19/Gtringle))
    );
    }
    glEnd ();

    int m;// 3rd left gombuj

    GLfloat x2=-0.460f; GLfloat h2= 0.515f; GLfloat Gr = 0.07f;
    int Gtringle1=40;

    GLfloat tp20 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x2,h2);
    for(m= 0;m<=Gtringle1; m++)
    {
    glVertex2f (
    x2+(Gr*cos(m*tp20/Gtringle1)),
    h2+(Gr*sin(m*tp20/Gtringle1))
    );
    }
    glEnd ();

    int n;// 4th left gombuj

    GLfloat x3=-0.315f; GLfloat h3= 0.515f; GLfloat Gr1 = 0.07f;
    int Gtringle21=40;

    GLfloat tp21 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x3,h3);
    for(n= 0;n<=Gtringle21; n++)
    {
    glVertex2f (
        x3+(Gr1*cos(n*tp21/Gtringle21)),
        h3+(Gr1*sin(n*tp21/Gtringle21))
    );
    }
    glEnd ();

    int p;// 5th left gombuj

    GLfloat x4=-0.170f; GLfloat h4= 0.515f; GLfloat Gr2 = 0.07f;
    int Gtringle22=40;

    GLfloat tp22=1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x4,h4);
    for(p= 0;p<=Gtringle22; p++)
    {
    glVertex2f (
    x4+(Gr2*cos(p*tp22/Gtringle22)),
    h4+(Gr2*sin(p*tp22/Gtringle22))
    );
    }
    glEnd ();

    int q;// 6th left gombuj

    GLfloat x5=-0.028f; GLfloat h5= 0.515f; GLfloat Gr3 = 0.07f;
    int Gtringle23=40;

    GLfloat tp23 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x5,h5);
    for(q= 0;q<=Gtringle23; q++)
    {
    glVertex2f (
    x5+(Gr3*cos(q*tp23/Gtringle23)),
    h5+(Gr3*sin(q*tp23/Gtringle23))
    );
    }
    glEnd ();

    glBegin(GL_TRIANGLES);//right side  small tree triangle-1
   glColor3ub(47, 92, 13);
    glVertex2f(0.00f, -0.35f);//middle line
    glVertex2f(0.10f, -0.35f);
    glVertex2f(0.05f, -0.15f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-2
   glColor3ub(47, 92, 13);
    glVertex2f(0.05f, -0.25f);//middle line
    glVertex2f(0.10f, -0.4f);
    glVertex2f(0.00f, -0.4f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-3
   glColor3ub(47, 92, 13);
    glVertex2f(0.05f, -0.30f);//middle line
    glVertex2f(0.10f, -0.45f);
    glVertex2f(0.00f, -0.45f);

    glEnd();



    glBegin(GL_TRIANGLES);//left side  small tree triangle-1
    glColor3ub(47, 92, 13);
    glVertex2f(-0.55f, -0.35f);//middle line
    glVertex2f(-0.65f, -0.35f);
    glVertex2f(-0.60f, -0.15f);

    glEnd();
        glBegin(GL_TRIANGLES);//left side  small tree triangle-2
 glColor3ub(47, 92, 13);
    glVertex2f(-0.55f, -0.4f);//middle line
    glVertex2f(-0.65f, -0.4f);
    glVertex2f(-0.60f, -0.25f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  small tree triangle-3
glColor3ub(47, 92, 13);
    glVertex2f(-0.55f, -0.45f);//middle line
    glVertex2f(-0.65f, -0.45f);
    glVertex2f(-0.60f, -0.30f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side  middle tree triangle-1
glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -0.40f);//middle line
    glVertex2f(0.1f, -0.6f);
    glVertex2f(0.2f, -0.6f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-2
    glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -0.50f);//middle line
    glVertex2f(0.1f, -0.65f);
    glVertex2f(0.2f, -0.65f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-3
glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -0.55f);//middle line
    glVertex2f(0.1f, -0.7f);
    glVertex2f(0.2f, -0.7f);
    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -0.40f);//middle line
    glVertex2f(-0.75f, -0.60f);
    glVertex2f(-0.65f, -0.60f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -0.50f);//middle line
    glVertex2f(-0.75f, -0.65f);
    glVertex2f(-0.65f, -0.65f);

    glEnd();


    glBegin(GL_TRIANGLES);//left side  middle tree triangle-2
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -0.55f);//middle line
    glVertex2f(-0.75f, -0.70f);
    glVertex2f(-0.65f, -0.70f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side last tree triangle-1
   glColor3ub(47, 92, 13);
    glVertex2f(0.25f, -0.65f);//middle line
    glVertex2f(0.2f, -0.85f);
    glVertex2f(0.3f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side last tree triangle-2
    glColor3ub(47, 92, 13);
    glVertex2f(0.25f, -0.75f);//middle line
    glVertex2f(0.2f, -0.90f);
    glVertex2f(0.3f, -0.90f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  last tree triangle-3
    glColor3ub(47, 92, 13);
    glVertex2f(0.25f, -0.80f);//middle line
    glVertex2f(0.2f, -0.95f);
    glVertex2f(0.3f, -0.95f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-1
   glColor3ub(47, 92, 13);
    glVertex2f(-0.80f, -0.65f);//middle line
    glVertex2f(-0.85f, -0.85f);
    glVertex2f(-0.75f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  last tree triangle-2
   glColor3ub(47, 92, 13);
    glVertex2f(-0.80f, -0.75f);//middle line
    glVertex2f(-0.85f, -0.9f);
    glVertex2f(-0.75f, -0.9f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-3
    glColor3ub(47, 92, 13);
    glVertex2f(-0.80f, -0.8f);//middle line
    glVertex2f(-0.85f, -0.95f);
    glVertex2f(-0.75f, -0.95f);

    glEnd();



GLfloat p26=-0.95f; GLfloat q26= 0.7f; GLfloat r26 = 0.15f;// big tree
int tringle26=40;
GLfloat tp26 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub(47, 92, 13);
glVertex2f (p26,q26);
for(i= 0;i<=tringle26; i++)
{
glVertex2f(p26+(r26*cos(i*tp26/tringle26)),q26+(r26*sin(i*tp26/tringle26)));
}
glEnd ();


//int j; //Right Leaf
GLfloat p27=-0.75f; GLfloat q27= 0.7f; GLfloat r27 = 0.15f;
int tringle27=40;
GLfloat tp27 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub(47, 92, 13);
glVertex2f (p27,q27);
for(i= 0;i<=tringle27; i++)
{
glVertex2f (p27+(r27*cos(i*tp27/tringle27)),q27+(r27*sin(i*tp27/tringle27)));
}
glEnd ();


//int k; //Upper Leaf
GLfloat p28=-0.85f; GLfloat q28= 0.85f; GLfloat r28 = 0.15f;
int tringle28=40;
GLfloat tp28 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub(47, 92, 13);
glVertex2f (p28,q28);
for(i= 0;i<=tringle28; i++)
{
glVertex2f (
p28+(r28*cos(i*tp28/tringle28)),q28+(r28*sin(i*tp28/tringle28)));
}
glEnd ();


glBegin(GL_QUADS);//Tree ground
glColor3ub(58,24,16);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.85f, -0.2f);
glVertex2f(-0.95f, -0.2f);
glEnd();


glBegin(GL_QUADS); //Left Tributariness
glColor3ub(58,24,16);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-1.0f, 0.6f);
glVertex2f(-0.95f, 0.6f);
glVertex2f(-0.85f, 0.5f);
glEnd();


glBegin(GL_QUADS); //Right Tributariness
glColor3ub(58,24,16);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.95f, 0.5f);
glEnd();


glBegin(GL_POLYGON); //Right Tributariness
glColor3ub(58,24,16);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.82f, 0.8f);
glVertex2f(-0.79f, 0.8f);
glVertex2f(-0.8f, 0.6f);
glEnd();


glBegin(GL_QUADS);
glColor3ub(58,24,16);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();


    glBegin(GL_QUADS);
glColor3ub(58,24,16);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();

/*glBegin(GL_POLYGON);// TREE BOTTOM SURFACE
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-1.0f, -0.15f);
glVertex2f(-1.0f, -0.3f);
glVertex2f(-0.95f, -0.25f);
glVertex2f(-0.95f, -0.20f);
glVertex2f(-0.85f, -0.20f);
glVertex2f(-0.85f, -0.25f);
glVertex2f(-0.80f, -0.30f);
glVertex2f(-0.80f, -0.15f);
glEnd();*/

glBegin(GL_QUADS);//road
glColor3ub(64,33,5);
//glColor3f(0.58f, 0.23f, 0.19f);
glVertex2f(-0.47f, -0.25f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(0.15f, -1.0f);
glVertex2f(-0.65f, -1.0f);

glEnd();

glBegin(GL_LINES);//road
glColor3ub(89,38,11);
glVertex2f(-0.475f, -0.30f);
glVertex2f(-0.0875f, -0.30f);
glVertex2f(-0.490f, -0.35f);
glVertex2f(-0.07f, -0.35f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.515f, -0.45f);
glVertex2f(-0.035f, -0.45f);
glVertex2f(-0.525f, -0.5f);
glVertex2f(-0.02f, -0.5f);
glVertex2f(-0.54f, -0.55f);
glVertex2f(0.00f, -0.55f);
glVertex2f(-0.554f, -0.6f);
glVertex2f(0.017f, -0.6f);
glVertex2f(-0.562f, -0.65f);
glVertex2f(0.0355f, -0.65f);
glVertex2f(-0.575f, -0.7f);
glVertex2f(0.052f, -0.7f);
glVertex2f(-0.587f, -0.75f);
glVertex2f(0.064f, -0.75f);
glVertex2f(-0.6f, -0.8f);
glVertex2f(0.084f, -0.8f);
glVertex2f(-0.615f, -0.85f);
glVertex2f(0.1f, -0.85f);
glVertex2f(-0.625f, -0.9f);
glVertex2f(0.12f, -0.9f);
glVertex2f(-0.64f, -0.95f);
glVertex2f(0.135f, -0.95f);
glVertex2f(-0.60f, -0.8f);
glVertex2f(-0.60f, -1.0f);
glVertex2f(-0.55f, -0.6f);
glVertex2f(-0.55f, -1.0f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.5f, -1.0f);
glVertex2f(-0.45f, -0.25f);
glVertex2f(-0.45f, -1.0f);
glVertex2f(-0.4f, -0.25f);
glVertex2f(-0.4f, -1.0f);
glVertex2f(-0.35f, -0.25f);
glVertex2f(-0.35f, -1.0f);
glVertex2f(-0.3f, -0.25f);
glVertex2f(-0.3f, -1.0f);
glVertex2f(-0.25f, -0.25f);
glVertex2f(-0.25f, -1.0f);
glVertex2f(-0.2f, -0.25f);
glVertex2f(-0.2f, -1.0f);
glVertex2f(-0.15f, -0.25f);
glVertex2f(-0.15f, -1.0f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(-0.1f, -1.0f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.05f, -1.0f);
glVertex2f(0.0f, -0.55f);
glVertex2f(0.0f, -1.0f);
glVertex2f(0.05f, -0.7f);
glVertex2f(0.05f, -1.0f);
glVertex2f(0.10f, -0.85f);
glVertex2f(0.10f, -1.0f);
glEnd();


    glBegin(GL_QUADS);//ghatpar
    glColor3f(0.21f, 0.16f, 0.10f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.85f, 0.15f);
    glVertex2f(0.75f, 0.15f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.71f, 0.0f);
    glVertex2f(0.71f, 0.08f);
    glVertex2f(0.70f, 0.08f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.72f, 0.060f);
    glVertex2f(0.73f, 0.060f);
    glVertex2f(0.73f, 0.12f);
    glVertex2f(0.72f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.74f, 0.12f);
    glVertex2f(0.75f, 0.12f);
    glVertex2f(0.75f, 0.18f);
    glVertex2f(0.74f, 0.18f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.890f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.9f, 0.08f);
    glVertex2f(0.890f, 0.08f);
    glEnd();

    glBegin(GL_QUADS);//ghatpar right middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.87f, 0.060f);
    glVertex2f(0.88f, 0.060f);
    glVertex2f(0.88f, 0.12f);
    glVertex2f(0.87f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.85f, 0.12f);
    glVertex2f(0.86f, 0.12f);
    glVertex2f(0.86f, 0.18f);
    glVertex2f(0.85f, 0.18f);
    glEnd();


  glFlush();
}

















//asr mood

void asr()
    { glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

     glBegin(GL_QUADS);//GROUND
    glColor3ub(82, 189, 15);
    glVertex2f(-1.0f, -1.0f);
    glVertex2f(1.0f, -1.0f);
    glVertex2f(1.0f, 0.55f);
    glVertex2f(-1.0f, 0.55f);
    glEnd();

    glBegin(GL_QUADS);// SKY
    glColor3f(.68f,.85f,.90f);
    glVertex2f(-1.0f, 0.55f);
    glColor3f(.68f,.85f,.90f);
    glVertex2f(1.0f, 0.55f);
    glColor3ub(129, 201, 219);
    glVertex2f(1.0f, 1.0f);
    glColor3ub(129, 201, 219);
    glVertex2f(-1.0f, 1.0f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(22, 82, 23);
    glVertex2f(-0.50f, 0.55f);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.40f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(22, 82, 23);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(-0.20f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(22, 82, 23);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(0.10f, 0.55f);
    glVertex2f(0.0f, 0.65f);
    glEnd();



    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-1.0f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.90f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.70f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.50f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(-0.30f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(-0.10f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain in near
    glColor3ub(8, 115, 10);
    glVertex2f(0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(0.10f, 0.65f);
    glEnd();


    //glPushMatrix();
    //glTranslatef(0.0f, position2, 0.0f);
    int x;//sun
    GLfloat p10 = 0.78f; GLfloat q10 = 0.7f;
    GLfloat r10 = 0.10f;
    int triangle10 = 40;
    GLfloat tp10 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    //glColor3ub(222, 82, 26);
    glColor3ub(253, 184, 19);
    glVertex2f(p10,q10);
    for(x = 0; x <= triangle10; x++){
        glVertex2f(p10+(r10*cos(x*tp10/triangle10)), q10+(r10*sin(x*tp10/triangle10)));
    }

    glEnd();
       // glPopMatrix();


    glBegin(GL_POLYGON);//river
    //glColor3f(0.51f, 0.44f, 1.0f);

    glColor3f(0.40f, 0.71f, 1.0f);
    glVertex2f(-1.0f, 0.10f);
    glVertex2f(1.0f, 0.10f);

    //glColor3ub(32, 76, 140);
    glColor3f(0.0f, 0.60f, 0.88f);
    glVertex2f(1.0f, 0.55f);
    glColor3f(0.0f, 0.60f, 0.88f);
    glVertex2f(-1.0f, 0.55f);
    glEnd();


    glPushMatrix();
    glTranslatef(position,0.0f, 0.0f);
    glBegin(GL_QUADS);// streamer ist part
    glColor3f(0.36f, 0.20f, 0.09f);
    glVertex2f(0.70f, 0.125f);
    glVertex2f(0.90f, 0.125f);
    glVertex2f(0.95f, 0.175f);
    glVertex2f(0.65f, 0.175f);
    glEnd();

    glBegin(GL_QUADS);//2nd part
    glColor3f(0.0f, 0.45f, 0.26f);
    glVertex2f(0.70f, 0.175f);
    glVertex2f(0.90f, 0.175f);
    glVertex2f(0.90f, 0.225f);
    glVertex2f(0.70f, 0.225f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.725f, 0.1875f);
    glVertex2f(0.75f, 0.1875f);
    glVertex2f(0.75f, 0.2125f);
    glVertex2f(0.725f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.775f, 0.1875f);
    glVertex2f(0.80f, 0.1875f);
    glVertex2f(0.80f, 0.2125f);
    glVertex2f(0.775f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.825f, 0.1875f);
    glVertex2f(0.85f, 0.1875f);
    glVertex2f(0.85f, 0.2125f);
    glVertex2f(0.825f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.875f, 0.1875f);
    glVertex2f(0.90f, 0.1875f);
    glVertex2f(0.90f, 0.2125f);
    glVertex2f(0.875f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);//3rd part
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.725f, 0.225f);
    glVertex2f(0.875f, 0.225f);
    glVertex2f(0.875f, 0.255f);
    glVertex2f(0.725f, 0.255f);
    glEnd();

    glBegin(GL_QUADS);// air out
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.745f, 0.255f);
    glVertex2f(0.760f, 0.255f);
    glVertex2f(0.760f, 0.305f);
    glVertex2f(0.745f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);// air out
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.775f, 0.255f);
    glVertex2f(0.79f, 0.255f);
    glVertex2f(0.79f, 0.305f);
    glVertex2f(0.775f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);// air out
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.805f, 0.255f);
    glVertex2f(0.82f, 0.255f);
    glVertex2f(0.82f, 0.305f);
    glVertex2f(0.805f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);// air out
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.835f, 0.255f);
    glVertex2f(0.85f, 0.255f);
    glVertex2f(0.85f, 0.305f);
    glVertex2f(0.835f, 0.305f);
    glEnd();

    glPopMatrix();


    glPushMatrix();
    glTranslatef(position3,0.0f, 0.0f);
    glBegin(GL_QUADS);//boat
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.47f, 0.325f);
    glVertex2f(0.65f, 0.325f);
    glVertex2f(0.70f, 0.375f);
    glVertex2f(0.42f, 0.375f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.49f, 0.375f);
    glVertex2f(0.49f, 0.475f);

    glVertex2f(0.50f, 0.375f);
    glVertex2f(0.50f, 0.525f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.55f, 0.375f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.57f, 0.375f);

    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.455f, 0.38f);
    glVertex2f(0.49f, 0.38f);
    glVertex2f(0.49f, 0.475f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.50f, 0.38f);
    glVertex2f(0.535f, 0.38f);
    glVertex2f(0.50f, 0.575f);
    glEnd();

    glBegin(GL_TRIANGLES);//maji
    glColor3ub(21, 176, 34);
    glVertex2f(0.63f, 0.40f);
    glVertex2f(0.67f, 0.40f);
    glVertex2f(0.65f, 0.45f);
    glEnd();

    int v;//maji

    GLfloat p24=0.65f; GLfloat q24= 0.45f; GLfloat r24 = 0.01f;
    int Gtringle24=40;

    GLfloat tp24 =2.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(207, 202, 155);
    glVertex2f (p24,q24);
    for(v= 0;v<=Gtringle24; v++)
    {
    glVertex2f (
    p24+(r24*cos(v*tp24/Gtringle24)),
    q24+(r24*sin(v*tp24/Gtringle24))
    );
    }
    glEnd ();

    glBegin(GL_LINES);//maji
    glColor3ub(207, 202, 155);
    glVertex2f(0.645f, 0.40f);
    glVertex2f(0.645f, 0.375f);

    glVertex2f(0.655f, 0.40f);
    glVertex2f(0.655f, 0.375f);

    glVertex2f(0.66f, 0.425f);
    glVertex2f(0.62f, 0.415f);

    glVertex2f(0.64f, 0.425f);
    glVertex2f(0.61f, 0.435f);

    glColor3ub(145, 86, 3);
    glVertex2f(0.61f, 0.445f);
    glVertex2f(0.66f, 0.30f);

    glEnd();
        glPopMatrix();


    glBegin(GL_QUADS);//Mosque
    glColor3f(0.74f, 0.45f, 0.26f);
    glVertex2f(-0.75f, -0.20f);
    glVertex2f(0.20f, -0.20f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(-0.75f, 0.50f);
    glEnd();

    glBegin(GL_QUADS);//Mosque upper border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.75f, 0.50f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(0.20f, 0.515f);
    glVertex2f(-0.75f, 0.515f);
    glEnd();

    glBegin(GL_QUADS);//Road
    glColor3f(0.97f, 0.98f, 1.0f);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.45f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow left
    glColor3f(0.27f, 0.55f, 0.0f);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(-0.65f, -1.0f);
    glVertex2f(-0.45f, -0.20f);
    glVertex2f(-0.47f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow right
    glColor3f(0.27f, 0.55f, 0.0f);
    glVertex2f(0.15f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.12f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Mosque bottom border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, -0.25f);
    glVertex2f(0.275f, -0.25f);
    glVertex2f(0.275f, -0.20f);
    glVertex2f(-0.825f, -0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.35f, -0.20f);
    glVertex2f(-0.20f, -0.20f);
    glVertex2f(-0.20f, 0.20f);
    glVertex2f(-0.225f, 0.30f);
    glVertex2f(-0.275f, 0.35f);
    glVertex2f(-0.325f, 0.30f);
    glVertex2f(-0.35f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.325f, -0.20f);
    glVertex2f(-0.21f, -0.20f);
    glVertex2f(-0.21f, 0.20f);
    glVertex2f(-0.228f, 0.28f);
    glVertex2f(-0.275f, 0.32f);
    glVertex2f(-0.31f, 0.28f);
    glVertex2f(-0.325f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.15f, -0.20f);
    glVertex2f(-0.05f, -0.20f);
    glVertex2f(-0.05f, 0.15f);
    glVertex2f(-0.075f, 0.2125f);
    glVertex2f(-0.10f, 0.25f);
    glVertex2f(-0.125f, 0.2125f);
    glVertex2f(-0.15f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.135f, -0.20f);
    glVertex2f(-0.058f, -0.20f);
    glVertex2f(-0.058f, 0.15f);
    glVertex2f(-0.085f, 0.2125f);
    glVertex2f(-0.10f, 0.225f);
    glVertex2f(-0.112f, 0.2125f);
    glVertex2f(-0.135f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.0f, -0.20f);
    glVertex2f(0.10f, -0.20f);
    glVertex2f(0.10f, 0.15f);
    glVertex2f(0.075f, 0.2125f);
    glVertex2f(0.050f, 0.25f);
    glVertex2f(0.025f, 0.2125f);
    glVertex2f(0.0f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.015f, -0.20f);
    glVertex2f(0.092f, -0.20f);
    glVertex2f(0.092f, 0.15f);
    glVertex2f(0.065f, 0.2125f);
    glVertex2f(0.050f, 0.225f);
    glVertex2f(0.037f, 0.2125f);
    glVertex2f(0.015f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.50f, -0.20f);
    glVertex2f(-0.40f, -0.20f);
    glVertex2f(-0.40f, 0.15f);
    glVertex2f(-0.425f, 0.2125f);
    glVertex2f(-0.45f, 0.25f);
    glVertex2f(-0.475f, 0.2125f);
    glVertex2f(-0.50f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.492f, -0.20f);
    glVertex2f(-0.415f, -0.20f);
    glVertex2f(-0.4155f, 0.15f);
    glVertex2f(-0.437f, 0.2125f);
    glVertex2f(-0.45f, 0.225f);
    glVertex2f(-0.465f, 0.2125f);
    glVertex2f(-0.492f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.65f, -0.20f);
    glVertex2f(-0.55f, -0.20f);
    glVertex2f(-0.55f, 0.15f);
    glVertex2f(-0.575f, 0.2125f);
    glVertex2f(-0.60f, 0.25f);
    glVertex2f(-0.625f, 0.2125f);
    glVertex2f(-0.65f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.642f, -0.20f);
    glVertex2f(-0.565f, -0.20f);
    glVertex2f(-0.565f, 0.15f);
    glVertex2f(-0.587f, 0.2125f);
    glVertex2f(-0.60f, 0.225f);
    glVertex2f(-0.615f, 0.2125f);
    glVertex2f(-0.642f, 0.15f);
    glEnd();


    glBegin(GL_QUADS);//left minar
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.80f, -0.20f);
    glVertex2f(-0.70f, -0.20f);
    glVertex2f(-0.70f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//left miner border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.25f);
    glVertex2f(-0.675f, 0.25f);
    glVertex2f(-0.675f, 0.30f);
    glVertex2f(-0.825f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.525f);
    glVertex2f(-0.675f, 0.525f);
    glVertex2f(-0.675f, 0.55f);
    glVertex2f(-0.825f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.78f, 0.30f);
    glVertex2f(-0.72f, 0.30f);
    glVertex2f(-0.72f, 0.45f);
    glVertex2f(-0.75f, 0.475f);
    glVertex2f(-0.78f, 0.45f);
    glEnd();

    int t;// Right 2nd gombuj

    GLfloat x7=0.11f; GLfloat h7= 0.515f; GLfloat Gr5 = 0.07f;
    int Gtringle25=40;

    GLfloat tp25 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x7,h7);
    for(t= 0;t<=Gtringle25; t++)
    {
    glVertex2f (
    x7+(Gr5*cos(t*tp25/Gtringle25)),
    h7+(Gr5*sin(t*tp25/Gtringle25))
    );
    }
    glEnd ();

    glBegin(GL_QUADS);//right minar
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.15f, -0.20f);
    glVertex2f(0.25f, -0.20f);
    glVertex2f(0.25f, 0.55f);
    glVertex2f(0.15f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//right miner border-1
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.25f);
    glVertex2f(0.275f, 0.25f);
    glVertex2f(0.275f, 0.30f);
    glVertex2f(0.125f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);//right miner border -2
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.525f);
    glVertex2f(0.275f, 0.525f);
    glVertex2f(0.275f, 0.55f);
    glVertex2f(0.125f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.170f, 0.30f);
    glVertex2f(0.230f, 0.30f);
    glVertex2f(0.230f, 0.45f);
    glVertex2f(0.20f, 0.475f);
    glVertex2f(0.170f, 0.45f);
    glEnd();

    glPushMatrix();
    glTranslatef(position4,0.0f, 0.0f);
    // 1st sky
    int i;
    GLfloat p1 = 0.20f; GLfloat q1 = 0.95f;
    GLfloat r1 = 0.05f;
    int triangle1 = 40;
    GLfloat tp1 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p1,q1);
    for(i = 0; i <= triangle1; i++){
        glVertex2f(p1+(r1*cos(i*tp1/triangle1)), q1+(r1*sin(i*tp1/triangle1)));
    }
    glEnd();

    GLfloat p2 = 0.32f; GLfloat q2 = 0.95f;
    GLfloat r2 = 0.10f;
    int triangle2 = 40;
    GLfloat tp2 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p2,q2);
    for(i = 0; i <= triangle2; i++){
        glVertex2f(p2+(r2*cos(i*tp2/triangle2)), q2+(r2*sin(i*tp2/triangle2)));
    }
    glEnd();

    GLfloat p3 = 0.45f; GLfloat q3 = 0.95f;
    GLfloat r3 = 0.06f;
    int triangle3 = 40;
    GLfloat tp3 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p3,q3);
    for(i = 0; i <= triangle3; i++){
        glVertex2f(p3+(r3*cos(i*tp3/triangle3)), q3+(r3*sin(i*tp3/triangle3)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position5,0.0f, 0.0f);
    //2nd sky
    GLfloat p4 = 0.03f; GLfloat q4 = 0.77f;
    GLfloat r4 = 0.05f;
    int triangle4 = 40;
    GLfloat tp4 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p4,q4);
    for(i = 0; i <= triangle4; i++){
        glVertex2f(p4+(r4*cos(i*tp4/triangle4)), q4+(r4*sin(i*tp4/triangle4)));
    }
    glEnd();

    GLfloat p5 = 0.15f; GLfloat q5 = 0.77f;
    GLfloat r5 = 0.10f;
    int triangle5 = 40;
    GLfloat tp5 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p5,q5);
    for(i = 0; i <= triangle5; i++){
        glVertex2f(p5+(r5*cos(i*tp5/triangle5)), q5+(r5*sin(i*tp5/triangle5)));
    }
    glEnd();

    GLfloat p6 = 0.28f; GLfloat q6 = 0.77f;
    GLfloat r6 = 0.06f;
    int triangle6 = 40;
    GLfloat tp6 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p6,q6);
    for(i = 0; i <= triangle6; i++){
        glVertex2f(p6+(r6*cos(i*tp6/triangle6)), q6+(r6*sin(i*tp6/triangle6)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position6,0.0f, 0.0f);
    //3rd sky
    GLfloat p7 = -0.25f; GLfloat q7 = 0.90f;
    GLfloat r7 = 0.05f;
    int triangle7 = 40;
    GLfloat tp7 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p7,q7);
    for(i = 0; i <= triangle7; i++){
        glVertex2f(p7+(r7*cos(i*tp7/triangle7)), q7+(r7*sin(i*tp7/triangle7)));
    }
    glEnd();

    GLfloat p8 = -0.13f; GLfloat q8 = 0.90f;
    GLfloat r8 = 0.10f;
    int triangle8 = 40;
    GLfloat tp8 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p8,q8);
    for(i = 0; i <= triangle8; i++){
        glVertex2f(p8+(r8*cos(i*tp8/triangle8)), q8+(r8*sin(i*tp8/triangle8)));
    }
    glEnd();

    GLfloat p9 = 0.0f; GLfloat q9 = 0.90f;
    GLfloat r9 = 0.06f;
    int triangle9 = 40;
    GLfloat tp9 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(255,255,255);
    glVertex2f(p9,q9);
    for(i = 0; i <= triangle9; i++){
        glVertex2f(p9+(r9*cos(i*tp9/triangle9)), q9+(r9*sin(i*tp9/triangle9)));
    }
    glEnd();
     glPopMatrix();


    glPushMatrix();
    glTranslatef(position1,0.0f, 0.0f);
    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.30f, 0.75f);
    glVertex2f(-0.285f, 0.765f);
    glVertex2f(-0.30f, 0.75f);
    glVertex2f(-0.315f, 0.765f);
    glEnd();

    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.40f, 0.70f);
    glVertex2f(-0.385f, 0.715f);
    glVertex2f(-0.40f, 0.70f);
    glVertex2f(-0.415f, 0.715f);
    glEnd();

    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.33f, 0.68f);
    glVertex2f(-0.315f, 0.695f);
    glVertex2f(-0.33f, 0.68f);
    glVertex2f(-0.345f, 0.695f);
    glEnd();

    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.38f, 0.76f);
    glVertex2f(-0.365f, 0.775f);
    glVertex2f(-0.38f, 0.76f);
    glVertex2f(-0.395f, 0.775f);
    glEnd();

    glBegin(GL_LINES);//bird
    glColor3ub(0, 0, 255);
    glVertex2f(-0.27f, 0.72f);
    glVertex2f(-0.255f, 0.735f);
    glVertex2f(-0.27f, 0.72f);
    glVertex2f(-0.285f, 0.735f);
    glEnd();
     glPopMatrix();



   int a;//under stand circle

    GLfloat p100=0.63f; GLfloat q100= -0.83f; GLfloat r100 = 0.08f;
    int tringle100=40;

    GLfloat tp100 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p100,q100);
    for(a= 0;a<=tringle100; a++)
    {
        glVertex2f (
                    p100+(r100*cos(a*tp100/tringle100)),
                    q100+(r100*sin(a*tp100/tringle100))
                    );
    }
    glEnd ();



    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.425f, -0.83f);
    glVertex2f(0.435f, -0.83f);
    glVertex2f(0.435f, -0.65f);
    glVertex2f(0.425f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.39f, -0.78f);
    glVertex2f(0.40f, -0.78f);
    glVertex2f(0.40f, -0.65f);
    glVertex2f(0.39f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.46f, -0.78f);
    glVertex2f(0.47f, -0.78f);
    glVertex2f(0.47f, -0.65f);
    glVertex2f(0.46f, -0.65f);
    glEnd();

    GLfloat p30=0.43f; GLfloat q30= -0.63f; GLfloat r30 = 0.05f;
    int tringle30=40;

    GLfloat tp30 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p30,q30);
    for(a= 0;a<=tringle30; a++)
    {
        glVertex2f (p30+(r30*cos(a*tp30/tringle30)),q30+(r30*sin(a*tp30/tringle30)));
    }
    glEnd ();


    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.825f, -0.83f);
    glVertex2f(0.835f, -0.83f);
    glVertex2f(0.835f, -0.65f);
    glVertex2f(0.825f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.79f, -0.78f);
    glVertex2f(0.80f, -0.78f);
    glVertex2f(0.80f, -0.65f);
    glVertex2f(0.79f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.86f, -0.78f);
    glVertex2f(0.87f, -0.78f);
    glVertex2f(0.87f, -0.65f);
    glVertex2f(0.86f, -0.65f);
    glEnd();

    GLfloat p31=0.83f; GLfloat q31= -0.63f; GLfloat r31 = 0.05f;
    int tringle31=40;

    GLfloat tp31 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p31,q31);
    for(a= 0;a<=tringle31; a++)
    {
        glVertex2f (p31+(r31*cos(a*tp31/tringle31)),q31+(r31*sin(a*tp31/tringle31)));
    }
    glEnd ();


    glBegin(GL_QUADS);//CHAIR
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.35f, -0.15f);
    glVertex2f(0.50f, -0.15f);
    glVertex2f(0.525f, -0.05f);
    glVertex2f(0.375f, -0.05f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.36f, -0.15f);
    glVertex2f(0.36f, -0.225f);

    glVertex2f(0.49f, -0.15f);
    glVertex2f(0.49f, -0.225f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, -0.145f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, 0.05f);

    glVertex2f(0.38f, -0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.025f);
    glVertex2f(0.38f, 0.025f);

    glVertex2f(0.52f, 0.00f);
    glVertex2f(0.38f, 0.00f);

    glEnd();



    glBegin(GL_QUADS);//table stand
    glColor3ub(112,128,144);
    glVertex2f(0.62f, -0.81f);
    glVertex2f(0.64f, -0.81f);
    glColor3ub(119,136,153);
    glVertex2f(0.64f, -0.64f);
    glVertex2f(0.62f, -0.64f);
    glEnd();

         int j;//bigger circle of foyara
    GLfloat p11=0.63f; GLfloat q11= -0.52f; GLfloat r11 = 0.132f;
    int tringle11=40;

    GLfloat tp11 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3f ( 0.46f,0.46f,0.46f);//grey color
    glVertex2f (p11,q11);
    for(j= 0;j<=tringle11; j++)
    {
    glVertex2f (
    p11+(r11*cos(j*tp11/tringle11)),
    q11+(r11*sin(j*tp11/tringle11))
    );
    }
    glEnd ();



    int k;//small circle foyara
    GLfloat p12=0.63f; GLfloat q12= -0.50f; GLfloat r12 = 0.115f;
    int tringle12=40;

    GLfloat tp12 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub (188,143,143);//blue color
    glVertex2f (p12,q12);
    for(k= 0;k<=tringle12; k++)
    {
    glVertex2f (
    p12+(r12*cos(k*tp12/tringle12)),
    q12+(r12*sin(k*tp12/tringle12))
    );
    }
    glEnd ();


    //left mic
    GLfloat p13 = -0.75f; GLfloat q13 = 0.375f;
    GLfloat r13 = 0.04f;
    int triangle13 = 40;
    GLfloat tp13 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p13,q13);
    for(i = 0; i <= triangle13; i++){
        glVertex2f(p13+(r13*cos(i*tp13/triangle13)), q13+(r13*sin(i*tp13/triangle13)));
    }
    glEnd();

    GLfloat p14 = -0.75f; GLfloat q14 = 0.375f;
    GLfloat r14 = 0.01f;
    int triangle14 = 40;
    GLfloat tp14 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p14,q14);
    for(i = 0; i <= triangle14; i++){
        glVertex2f(p14+(r14*cos(i*tp14/triangle14)), q14+(r14*sin(i*tp14/triangle14)));
    }
    glEnd();

    //right mic
    GLfloat p15 = 0.2f; GLfloat q15 = 0.375f;
    GLfloat r15 = 0.04f;
    int triangle15 = 40;
    GLfloat tp15 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p15,q15);
    for(i = 0; i <= triangle15; i++){
        glVertex2f(p15+(r15*cos(i*tp15/triangle15)), q15+(r15*sin(i*tp15/triangle15)));
    }
    glEnd();

    GLfloat p16 = 0.2f; GLfloat q16 = 0.375f;
    GLfloat r16 = 0.01f;
    int triangle16 = 40;
    GLfloat tp16 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p16,q16);
    for(i = 0; i <= triangle16; i++){
        glVertex2f(p16+(r16*cos(i*tp16/triangle16)), q16+(r16*sin(i*tp16/triangle16)));
    }
    glEnd();

   glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.30f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.28f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.28f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.32f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.32f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.29f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.095f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.105f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.325f, 0.25f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.305f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.23f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.345f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.27f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.24f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.350f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.33f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.28f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.370f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.32f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.29f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.90f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.88f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.28f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.92f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.32f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.29f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.095f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.105f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.925f, 0.25f);

    //flower
    glColor3ub(235, 235, 111);
    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.905f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.23f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.945f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.27f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.24f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.950f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.93f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.28f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.970f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.32f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.29f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.29f);

    glEnd();


    int l;//left minar gombuj

    GLfloat p17=-0.751f; GLfloat q17= 0.55f; GLfloat r17= 0.07f;
    int tringle17=40;

    GLfloat tp17 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (p17,q17);
    for(l= 0;l<=tringle17; l++)
    {
    glVertex2f (
    p17+(r17*cos(l*tp17/tringle17)),
    q17+(r17*sin(l*tp17/tringle17))
    );
    }
    glEnd ();

    int s;// Right minar gombuj

    GLfloat x18=0.2f; GLfloat h18= 0.55f; GLfloat Gr18 = 0.07f;
    int Gtringle18=40;

    GLfloat tp18 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x18,h18);
    for(s= 0;s<=Gtringle18; s++)
    {
    glVertex2f (
    x18+(Gr18*cos(s*tp18/Gtringle18)),
    h18+(Gr18*sin(s*tp18/Gtringle18))
    );
    }
    glEnd ();

    //2nd left gombuj
    GLfloat x1=-0.604f; GLfloat h1= 0.515f; GLfloat ra = 0.07f;
    int Gtringle=40;

    GLfloat tp19 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x1,h1);
    for(l= 0;l<=Gtringle; l++)
    {
    glVertex2f (
    x1+(ra*cos(l*tp19/Gtringle)),
    h1+(ra*sin(l*tp19/Gtringle))
    );
    }
    glEnd ();

    int m;// 3rd left gombuj

    GLfloat x2=-0.460f; GLfloat h2= 0.515f; GLfloat Gr = 0.07f;
    int Gtringle1=40;

    GLfloat tp20 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x2,h2);
    for(m= 0;m<=Gtringle1; m++)
    {
    glVertex2f (
    x2+(Gr*cos(m*tp20/Gtringle1)),
    h2+(Gr*sin(m*tp20/Gtringle1))
    );
    }
    glEnd ();

    int n;// 4th left gombuj

    GLfloat x3=-0.315f; GLfloat h3= 0.515f; GLfloat Gr1 = 0.07f;
    int Gtringle21=40;

    GLfloat tp21 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x3,h3);
    for(n= 0;n<=Gtringle21; n++)
    {
    glVertex2f (
        x3+(Gr1*cos(n*tp21/Gtringle21)),
        h3+(Gr1*sin(n*tp21/Gtringle21))
    );
    }
    glEnd ();

    int p;// 5th left gombuj

    GLfloat x4=-0.170f; GLfloat h4= 0.515f; GLfloat Gr2 = 0.07f;
    int Gtringle22=40;

    GLfloat tp22=1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x4,h4);
    for(p= 0;p<=Gtringle22; p++)
    {
    glVertex2f (
    x4+(Gr2*cos(p*tp22/Gtringle22)),
    h4+(Gr2*sin(p*tp22/Gtringle22))
    );
    }
    glEnd ();

    int q;// 6th left gombuj

    GLfloat x5=-0.028f; GLfloat h5= 0.515f; GLfloat Gr3 = 0.07f;
    int Gtringle23=40;

    GLfloat tp23 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x5,h5);
    for(q= 0;q<=Gtringle23; q++)
    {
    glVertex2f (
    x5+(Gr3*cos(q*tp23/Gtringle23)),
    h5+(Gr3*sin(q*tp23/Gtringle23))
    );
    }
    glEnd ();

    glBegin(GL_TRIANGLES);//right side  small tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.00f, -0.35f);//middle line
    glVertex2f(0.10f, -0.35f);
    glVertex2f(0.05f, -0.15f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.05f, -0.25f);//middle line
    glVertex2f(0.10f, -0.4f);
    glVertex2f(0.00f, -0.4f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.05f, -0.30f);//middle line
    glVertex2f(0.10f, -0.45f);
    glVertex2f(0.00f, -0.45f);

    glEnd();



    glBegin(GL_TRIANGLES);//left side  small tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.55f, -0.35f);//middle line
    glVertex2f(-0.65f, -0.35f);
    glVertex2f(-0.60f, -0.15f);

    glEnd();
        glBegin(GL_TRIANGLES);//left side  small tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.55f, -0.4f);//middle line
    glVertex2f(-0.65f, -0.4f);
    glVertex2f(-0.60f, -0.25f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  small tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.55f, -0.45f);//middle line
    glVertex2f(-0.65f, -0.45f);
    glVertex2f(-0.60f, -0.30f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side  middle tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.15f, -0.40f);//middle line
    glVertex2f(0.1f, -0.6f);
    glVertex2f(0.2f, -0.6f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.15f, -0.50f);//middle line
    glVertex2f(0.1f, -0.65f);
    glVertex2f(0.2f, -0.65f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.15f, -0.55f);//middle line
    glVertex2f(0.1f, -0.7f);
    glVertex2f(0.2f, -0.7f);
    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.70f, -0.40f);//middle line
    glVertex2f(-0.75f, -0.60f);
    glVertex2f(-0.65f, -0.60f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.70f, -0.50f);//middle line
    glVertex2f(-0.75f, -0.65f);
    glVertex2f(-0.65f, -0.65f);

    glEnd();


    glBegin(GL_TRIANGLES);//left side  middle tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.70f, -0.55f);//middle line
    glVertex2f(-0.75f, -0.70f);
    glVertex2f(-0.65f, -0.70f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side last tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.25f, -0.65f);//middle line
    glVertex2f(0.2f, -0.85f);
    glVertex2f(0.3f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side last tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.25f, -0.75f);//middle line
    glVertex2f(0.2f, -0.90f);
    glVertex2f(0.3f, -0.90f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  last tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(0.25f, -0.80f);//middle line
    glVertex2f(0.2f, -0.95f);
    glVertex2f(0.3f, -0.95f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-1
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.80f, -0.65f);//middle line
    glVertex2f(-0.85f, -0.85f);
    glVertex2f(-0.75f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  last tree triangle-2
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.80f, -0.75f);//middle line
    glVertex2f(-0.85f, -0.9f);
    glVertex2f(-0.75f, -0.9f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-3
    glColor3f(0.0f, 0.60f, 0.0f);
    glVertex2f(-0.80f, -0.8f);//middle line
    glVertex2f(-0.85f, -0.95f);
    glVertex2f(-0.75f, -0.95f);

    glEnd();




GLfloat p26=-0.95f; GLfloat q26= 0.7f; GLfloat r26 = 0.15f;// big tree
int tringle26=40;
GLfloat tp26 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub ( 0,130,05);
glVertex2f (p26,q26);
for(i= 0;i<=tringle26; i++)
{
glVertex2f(p26+(r26*cos(i*tp26/tringle26)),q26+(r26*sin(i*tp26/tringle26)));
}
glEnd ();

//int j; //Right Leaf
GLfloat p27=-0.75f; GLfloat q27= 0.7f; GLfloat r27 = 0.15f;
int tringle27=40;
GLfloat tp27 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub ( 0,110,0);
glVertex2f (p27,q27);
for(i= 0;i<=tringle27; i++)
{
glVertex2f (p27+(r27*cos(i*tp27/tringle27)),q27+(r27*sin(i*tp27/tringle27)));
}
glEnd ();




//int k; //Upper Leaf
GLfloat p28=-0.85f; GLfloat q28= 0.85f; GLfloat r28 = 0.15f;
int tringle28=40;
GLfloat tp28 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub ( 0,100,0);
glVertex2f (p28,q28);
for(i= 0;i<=tringle28; i++)
{
glVertex2f (
p28+(r28*cos(i*tp28/tringle28)),q28+(r28*sin(i*tp28/tringle28)));
}
glEnd ();


glBegin(GL_QUADS);//Tree ground
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.85f, -0.2f);
glVertex2f(-0.95f, -0.2f);
glEnd();



glBegin(GL_QUADS); //Left Tributariness
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-1.0f, 0.6f);
glVertex2f(-0.95f, 0.6f);
glVertex2f(-0.85f, 0.5f);
glEnd();


glBegin(GL_QUADS); //Right Tributariness
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.95f, 0.5f);
glEnd();


glBegin(GL_POLYGON); //Right Tributariness
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.82f, 0.8f);
glVertex2f(-0.79f, 0.8f);
glVertex2f(-0.8f, 0.6f);
glEnd();


glBegin(GL_QUADS);
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();


    glBegin(GL_QUADS);
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();

/*glBegin(GL_POLYGON);// TREE BOTTOM SURFACE
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-1.0f, -0.15f);
glVertex2f(-1.0f, -0.3f);
glVertex2f(-0.95f, -0.25f);
glVertex2f(-0.95f, -0.20f);
glVertex2f(-0.85f, -0.20f);
glVertex2f(-0.85f, -0.25f);
glVertex2f(-0.80f, -0.30f);
glVertex2f(-0.80f, -0.15f);
glEnd();*/

glBegin(GL_QUADS);//road
glColor3f(1.0f, 0.6f, 0.09f);
//glColor3f(0.58f, 0.23f, 0.19f);
glVertex2f(-0.47f, -0.25f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(0.15f, -1.0f);
glVertex2f(-0.65f, -1.0f);

glEnd();

glBegin(GL_LINES);//road
glColor3f(0.87f, 0.68f, 0.45f);
glVertex2f(-0.475f, -0.30f);
glVertex2f(-0.0875f, -0.30f);
glVertex2f(-0.490f, -0.35f);
glVertex2f(-0.07f, -0.35f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.515f, -0.45f);
glVertex2f(-0.035f, -0.45f);
glVertex2f(-0.525f, -0.5f);
glVertex2f(-0.02f, -0.5f);
glVertex2f(-0.54f, -0.55f);
glVertex2f(0.00f, -0.55f);
glVertex2f(-0.554f, -0.6f);
glVertex2f(0.017f, -0.6f);
glVertex2f(-0.562f, -0.65f);
glVertex2f(0.0355f, -0.65f);
glVertex2f(-0.575f, -0.7f);
glVertex2f(0.052f, -0.7f);
glVertex2f(-0.587f, -0.75f);
glVertex2f(0.064f, -0.75f);
glVertex2f(-0.6f, -0.8f);
glVertex2f(0.084f, -0.8f);
glVertex2f(-0.615f, -0.85f);
glVertex2f(0.1f, -0.85f);
glVertex2f(-0.625f, -0.9f);
glVertex2f(0.12f, -0.9f);
glVertex2f(-0.64f, -0.95f);
glVertex2f(0.135f, -0.95f);
glVertex2f(-0.60f, -0.8f);
glVertex2f(-0.60f, -1.0f);
glVertex2f(-0.55f, -0.6f);
glVertex2f(-0.55f, -1.0f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.5f, -1.0f);
glVertex2f(-0.45f, -0.25f);
glVertex2f(-0.45f, -1.0f);
glVertex2f(-0.4f, -0.25f);
glVertex2f(-0.4f, -1.0f);
glVertex2f(-0.35f, -0.25f);
glVertex2f(-0.35f, -1.0f);
glVertex2f(-0.3f, -0.25f);
glVertex2f(-0.3f, -1.0f);
glVertex2f(-0.25f, -0.25f);
glVertex2f(-0.25f, -1.0f);
glVertex2f(-0.2f, -0.25f);
glVertex2f(-0.2f, -1.0f);
glVertex2f(-0.15f, -0.25f);
glVertex2f(-0.15f, -1.0f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(-0.1f, -1.0f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.05f, -1.0f);
glVertex2f(0.0f, -0.55f);
glVertex2f(0.0f, -1.0f);
glVertex2f(0.05f, -0.7f);
glVertex2f(0.05f, -1.0f);
glVertex2f(0.10f, -0.85f);
glVertex2f(0.10f, -1.0f);
glEnd();



    glBegin(GL_QUADS);//ghatpar
    glColor3f(0.21f, 0.16f, 0.10f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.85f, 0.15f);
    glVertex2f(0.75f, 0.15f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.71f, 0.0f);
    glVertex2f(0.71f, 0.08f);
    glVertex2f(0.70f, 0.08f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.72f, 0.060f);
    glVertex2f(0.73f, 0.060f);
    glVertex2f(0.73f, 0.12f);
    glVertex2f(0.72f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.74f, 0.12f);
    glVertex2f(0.75f, 0.12f);
    glVertex2f(0.75f, 0.18f);
    glVertex2f(0.74f, 0.18f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.890f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.9f, 0.08f);
    glVertex2f(0.890f, 0.08f);
    glEnd();

    glBegin(GL_QUADS);//ghatpar right middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.87f, 0.060f);
    glVertex2f(0.88f, 0.060f);
    glVertex2f(0.88f, 0.12f);
    glVertex2f(0.87f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.85f, 0.12f);
    glVertex2f(0.86f, 0.12f);
    glVertex2f(0.86f, 0.18f);
    glVertex2f(0.85f, 0.18f);
    glEnd();




    glFlush();
}



















//maghrib mood

void maghrib()
    { glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glBegin(GL_QUADS);//GROUND
    glColor3ub(0,128,0);
    glVertex2f(-1.0f, -1.0f);
    glVertex2f(1.0f, -1.0f);
    glVertex2f(1.0f, 0.55f);
    glVertex2f(-1.0f, 0.55f);
    glEnd();

    glBegin(GL_QUADS);// SKY
     glColor3ub(0,191,255);
   // glColor3f(.28f, .31f, .42f);
    glVertex2f(-1.0f, 0.55f);
    glColor3ub(255,140,0);
    //glColor3f(.28f, .31f, .42f);
    glVertex2f(1.0f, 0.55f);
       glColor3ub(0,0,139);
    //glColor3f(.10f,.10f,.13f);
    glVertex2f(1.0f, 1.0f);
    //glColor3f(.10f,.10f,.13f);
    glVertex2f(-1.0f, 1.0f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(51,112,93);
    glVertex2f(-0.50f, 0.55f);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.40f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(51,112,93);
    glVertex2f(-0.30f, 0.55f);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(-0.20f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(51,112,93);
    glVertex2f(-0.10f, 0.55f);
    glVertex2f(0.10f, 0.55f);
    glVertex2f(0.0f, 0.65f);
    glEnd();


    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(8, 115, 10);
    glVertex2f(-1.0f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.90f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.80f, 0.55f);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.70f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.60f, 0.55f);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.50f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.40f, 0.55f);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(-0.30f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(-0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(-0.10f, 0.65f);
    glEnd();

    glBegin(GL_TRIANGLES);//mountain
    glColor3ub(6, 78, 64);
    glVertex2f(0.20f, 0.55f);
    glVertex2f(0.0f, 0.55f);
    glVertex2f(0.10f, 0.65f);
    glEnd();


    //glPushMatrix();
   // glTranslatef(0.0f, position2, 0.0f);
    int x;//sun
    GLfloat p10 = 0.78f; GLfloat q10 = 0.5f;
    GLfloat r10 = 0.10f;
    int triangle10 = 40;
    GLfloat tp10 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(196, 78, 14);
    //glColor3ub(255, 255, 0);
    glVertex2f(p10,q10);
    for(x = 0; x <= triangle10; x++){
        glVertex2f(p10+(r10*cos(x*tp10/triangle10)), q10+(r10*sin(x*tp10/triangle10)));
    }

    glEnd();
       // glPopMatrix();



    glBegin(GL_POLYGON);//river
    //glColor3ub(86, 98, 133);
       // glColor3ub(30,144,255);
 glColor3ub(0,0,139);
    //glColor3f(0.40f, 0.71f, 1.0f);
    glVertex2f(-1.0f, 0.10f);
   // glColor3ub(86, 98, 133);

    glVertex2f(1.0f, 0.10f);
   glColor3ub(30,144,255);
    //glColor3ub(40, 45, 61);
    //glColor3f(.72f, .93f, .122f);
    glVertex2f(1.0f, 0.55f);
        //glColor3f(.72f, .93f, .122f);

    //glColor3ub(40, 45, 61);
    glVertex2f(-1.0f, 0.55f);
    glEnd();



    glPushMatrix();
    glTranslatef(position,0.0f, 0.0f);
    glBegin(GL_QUADS);// streamer
    glColor3f(0.36f, 0.20f, 0.09f);
    glVertex2f(0.70f, 0.125f);
    glVertex2f(0.90f, 0.125f);
    glVertex2f(0.95f, 0.175f);
    glVertex2f(0.65f, 0.175f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.45f, 0.26f);
    glVertex2f(0.70f, 0.175f);
    glVertex2f(0.90f, 0.175f);
    glVertex2f(0.90f, 0.225f);
    glVertex2f(0.70f, 0.225f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.725f, 0.1875f);
    glVertex2f(0.75f, 0.1875f);
    glVertex2f(0.75f, 0.2125f);
    glVertex2f(0.725f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.775f, 0.1875f);
    glVertex2f(0.80f, 0.1875f);
    glVertex2f(0.80f, 0.2125f);
    glVertex2f(0.775f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.825f, 0.1875f);
    glVertex2f(0.85f, 0.1875f);
    glVertex2f(0.85f, 0.2125f);
    glVertex2f(0.825f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);// streamer window
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.875f, 0.1875f);
    glVertex2f(0.90f, 0.1875f);
    glVertex2f(0.90f, 0.2125f);
    glVertex2f(0.875f, 0.2125f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.725f, 0.225f);
    glVertex2f(0.875f, 0.225f);
    glVertex2f(0.875f, 0.255f);
    glVertex2f(0.725f, 0.255f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.745f, 0.255f);
    glVertex2f(0.760f, 0.255f);
    glVertex2f(0.760f, 0.305f);
    glVertex2f(0.745f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.775f, 0.255f);
    glVertex2f(0.79f, 0.255f);
    glVertex2f(0.79f, 0.305f);
    glVertex2f(0.775f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.805f, 0.255f);
    glVertex2f(0.82f, 0.255f);
    glVertex2f(0.82f, 0.305f);
    glVertex2f(0.805f, 0.305f);
    glEnd();

    glBegin(GL_QUADS);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.835f, 0.255f);
    glVertex2f(0.85f, 0.255f);
    glVertex2f(0.85f, 0.305f);
    glVertex2f(0.835f, 0.305f);
    glEnd();

    glPopMatrix();


    glPushMatrix();
    glTranslatef(position3,0.0f, 0.0f);
    glBegin(GL_QUADS);//boat
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.47f, 0.325f);
    glVertex2f(0.65f, 0.325f);
    glVertex2f(0.70f, 0.375f);
    glVertex2f(0.42f, 0.375f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.49f, 0.375f);
    glVertex2f(0.49f, 0.475f);

    glVertex2f(0.50f, 0.375f);
    glVertex2f(0.50f, 0.525f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.55f, 0.375f);

    glVertex2f(0.50f, 0.575f);
    glVertex2f(0.57f, 0.375f);

    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.455f, 0.38f);
    glVertex2f(0.49f, 0.38f);
    glVertex2f(0.49f, 0.475f);
    glEnd();

    glBegin(GL_TRIANGLES);
    glColor3f(0.0f, 0.f, 0.26f);
    glVertex2f(0.50f, 0.38f);
    glVertex2f(0.535f, 0.38f);
    glVertex2f(0.50f, 0.575f);
    glEnd();

    glBegin(GL_TRIANGLES);//maji
    glColor3ub(21, 176, 34);
    glVertex2f(0.63f, 0.40f);
    glVertex2f(0.67f, 0.40f);
    glVertex2f(0.65f, 0.45f);
    glEnd();

    int v;//maji

    GLfloat p24=0.65f; GLfloat q24= 0.45f; GLfloat r24 = 0.01f;
    int Gtringle24=40;

    GLfloat tp24 =2.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(207, 202, 155);
    glVertex2f (p24,q24);
    for(v= 0;v<=Gtringle24; v++)
    {
    glVertex2f (
    p24+(r24*cos(v*tp24/Gtringle24)),
    q24+(r24*sin(v*tp24/Gtringle24))
    );
    }
    glEnd ();

    glBegin(GL_LINES);//maji
    glColor3ub(207, 202, 155);
    glVertex2f(0.645f, 0.40f);
    glVertex2f(0.645f, 0.375f);

    glVertex2f(0.655f, 0.40f);
    glVertex2f(0.655f, 0.375f);

    glVertex2f(0.66f, 0.425f);
    glVertex2f(0.62f, 0.415f);

    glVertex2f(0.64f, 0.425f);
    glVertex2f(0.61f, 0.435f);

    glColor3ub(145, 86, 3);
    glVertex2f(0.61f, 0.445f);
    glVertex2f(0.66f, 0.30f);

    glEnd();
        glPopMatrix();


    glBegin(GL_QUADS);//Mosque
    glColor3f(0.74f, 0.45f, 0.26f);
    glVertex2f(-0.75f, -0.20f);
    glVertex2f(0.20f, -0.20f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(-0.75f, 0.50f);
    glEnd();

    glBegin(GL_QUADS);//Mosque upper border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.75f, 0.50f);
    glVertex2f(0.20f, 0.50f);
    glVertex2f(0.20f, 0.515f);
    glVertex2f(-0.75f, 0.515f);
    glEnd();

    glBegin(GL_QUADS);//Road
    glColor3f(0.97f, 0.98f, 1.0f);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.45f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow left
    glColor3f(0.27f, 0.55f, 0.0f);
    glVertex2f(-0.70f, -1.0f);
    glVertex2f(-0.65f, -1.0f);
    glVertex2f(-0.45f, -0.20f);
    glVertex2f(-0.47f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Road side gass shadow right
    glColor3f(0.27f, 0.55f, 0.0f);
    glVertex2f(0.15f, -1.0f);
    glVertex2f(0.20f, -1.0f);
    glVertex2f(-0.10f, -0.20f);
    glVertex2f(-0.12f, -0.20f);
    glEnd();

    glBegin(GL_QUADS);//Mosque bottom border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, -0.25f);
    glVertex2f(0.275f, -0.25f);
    glVertex2f(0.275f, -0.20f);
    glVertex2f(-0.825f, -0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.35f, -0.20f);
    glVertex2f(-0.20f, -0.20f);
    glVertex2f(-0.20f, 0.20f);
    glVertex2f(-0.225f, 0.30f);
    glVertex2f(-0.275f, 0.35f);
    glVertex2f(-0.325f, 0.30f);
    glVertex2f(-0.35f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//Big gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.325f, -0.20f);
    glVertex2f(-0.21f, -0.20f);
    glVertex2f(-0.21f, 0.20f);
    glVertex2f(-0.228f, 0.28f);
    glVertex2f(-0.275f, 0.32f);
    glVertex2f(-0.31f, 0.28f);
    glVertex2f(-0.325f, 0.20f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.15f, -0.20f);
    glVertex2f(-0.05f, -0.20f);
    glVertex2f(-0.05f, 0.15f);
    glVertex2f(-0.075f, 0.2125f);
    glVertex2f(-0.10f, 0.25f);
    glVertex2f(-0.125f, 0.2125f);
    glVertex2f(-0.15f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.135f, -0.20f);
    glVertex2f(-0.058f, -0.20f);
    glVertex2f(-0.058f, 0.15f);
    glVertex2f(-0.085f, 0.2125f);
    glVertex2f(-0.10f, 0.225f);
    glVertex2f(-0.112f, 0.2125f);
    glVertex2f(-0.135f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.0f, -0.20f);
    glVertex2f(0.10f, -0.20f);
    glVertex2f(0.10f, 0.15f);
    glVertex2f(0.075f, 0.2125f);
    glVertex2f(0.050f, 0.25f);
    glVertex2f(0.025f, 0.2125f);
    glVertex2f(0.0f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//right last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.015f, -0.20f);
    glVertex2f(0.092f, -0.20f);
    glVertex2f(0.092f, 0.15f);
    glVertex2f(0.065f, 0.2125f);
    glVertex2f(0.050f, 0.225f);
    glVertex2f(0.037f, 0.2125f);
    glVertex2f(0.015f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.50f, -0.20f);
    glVertex2f(-0.40f, -0.20f);
    glVertex2f(-0.40f, 0.15f);
    glVertex2f(-0.425f, 0.2125f);
    glVertex2f(-0.45f, 0.25f);
    glVertex2f(-0.475f, 0.2125f);
    glVertex2f(-0.50f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left 1st gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.492f, -0.20f);
    glVertex2f(-0.415f, -0.20f);
    glVertex2f(-0.4155f, 0.15f);
    glVertex2f(-0.437f, 0.2125f);
    glVertex2f(-0.45f, 0.225f);
    glVertex2f(-0.465f, 0.2125f);
    glVertex2f(-0.492f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate outside
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.65f, -0.20f);
    glVertex2f(-0.55f, -0.20f);
    glVertex2f(-0.55f, 0.15f);
    glVertex2f(-0.575f, 0.2125f);
    glVertex2f(-0.60f, 0.25f);
    glVertex2f(-0.625f, 0.2125f);
    glVertex2f(-0.65f, 0.15f);
    glEnd();

    glBegin(GL_POLYGON);//left last gate inside
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.642f, -0.20f);
    glVertex2f(-0.565f, -0.20f);
    glVertex2f(-0.565f, 0.15f);
    glVertex2f(-0.587f, 0.2125f);
    glVertex2f(-0.60f, 0.225f);
    glVertex2f(-0.615f, 0.2125f);
    glVertex2f(-0.642f, 0.15f);
    glEnd();


    glBegin(GL_QUADS);//left minar
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(-0.80f, -0.20f);
    glVertex2f(-0.70f, -0.20f);
    glVertex2f(-0.70f, 0.55f);
    glVertex2f(-0.80f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//left miner border
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.25f);
    glVertex2f(-0.675f, 0.25f);
    glVertex2f(-0.675f, 0.30f);
    glVertex2f(-0.825f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(-0.825f, 0.525f);
    glVertex2f(-0.675f, 0.525f);
    glVertex2f(-0.675f, 0.55f);
    glVertex2f(-0.825f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(-0.78f, 0.30f);
    glVertex2f(-0.72f, 0.30f);
    glVertex2f(-0.72f, 0.45f);
    glVertex2f(-0.75f, 0.475f);
    glVertex2f(-0.78f, 0.45f);
    glEnd();

    int t;// Right 2nd gombuj

    GLfloat x7=0.11f; GLfloat h7= 0.515f; GLfloat Gr5 = 0.07f;
    int Gtringle25=40;

    GLfloat tp25 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x7,h7);
    for(t= 0;t<=Gtringle25; t++)
    {
    glVertex2f (
    x7+(Gr5*cos(t*tp25/Gtringle25)),
    h7+(Gr5*sin(t*tp25/Gtringle25))
    );
    }
    glEnd ();

    glBegin(GL_QUADS);//right minar
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.15f, -0.20f);
    glVertex2f(0.25f, -0.20f);
    glVertex2f(0.25f, 0.55f);
    glVertex2f(0.15f, 0.55f);
    glEnd();
    glBegin(GL_QUADS);//right miner border-1
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.25f);
    glVertex2f(0.275f, 0.25f);
    glVertex2f(0.275f, 0.30f);
    glVertex2f(0.125f, 0.30f);
    glEnd();
    glBegin(GL_QUADS);//right miner border -2
    glColor3f(0.54f, 0.21f, 0.060f);
    glVertex2f(0.125f, 0.525f);
    glVertex2f(0.275f, 0.525f);
    glVertex2f(0.275f, 0.55f);
    glVertex2f(0.125f, 0.55f);
    glEnd();
    glBegin(GL_POLYGON);//window
    glColor3f(0.59, 0.27f, 0.080f);
    glVertex2f(0.170f, 0.30f);
    glVertex2f(0.230f, 0.30f);
    glVertex2f(0.230f, 0.45f);
    glVertex2f(0.20f, 0.475f);
    glVertex2f(0.170f, 0.45f);
    glEnd();

    glPushMatrix();
    glTranslatef(position4,0.0f, 0.0f);
    // 1st sky
    int i;
    GLfloat p1 = 0.20f; GLfloat q1 = 0.95f;
    GLfloat r1 = 0.05f;
    int triangle1 = 40;
    GLfloat tp1 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(176,196,222);
    glVertex2f(p1,q1);
    for(i = 0; i <= triangle1; i++){
        glVertex2f(p1+(r1*cos(i*tp1/triangle1)), q1+(r1*sin(i*tp1/triangle1)));
    }
    glEnd();

    GLfloat p2 = 0.32f; GLfloat q2 = 0.95f;
    GLfloat r2 = 0.10f;
    int triangle2 = 40;
    GLfloat tp2 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(176,196,222);
    glVertex2f(p2,q2);
    for(i = 0; i <= triangle2; i++){
        glVertex2f(p2+(r2*cos(i*tp2/triangle2)), q2+(r2*sin(i*tp2/triangle2)));
    }
    glEnd();

    GLfloat p3 = 0.45f; GLfloat q3 = 0.95f;
    GLfloat r3 = 0.06f;
    int triangle3 = 40;
    GLfloat tp3 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(176,196,222);
    glVertex2f(p3,q3);
    for(i = 0; i <= triangle3; i++){
        glVertex2f(p3+(r3*cos(i*tp3/triangle3)), q3+(r3*sin(i*tp3/triangle3)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position5,0.0f, 0.0f);
    //2nd sky
    GLfloat p4 = 0.03f; GLfloat q4 = 0.77f;
    GLfloat r4 = 0.05f;
    int triangle4 = 40;
    GLfloat tp4 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(176,196,222);
    glVertex2f(p4,q4);
    for(i = 0; i <= triangle4; i++){
        glVertex2f(p4+(r4*cos(i*tp4/triangle4)), q4+(r4*sin(i*tp4/triangle4)));
    }
    glEnd();

    GLfloat p5 = 0.15f; GLfloat q5 = 0.77f;
    GLfloat r5 = 0.10f;
    int triangle5 = 40;
    GLfloat tp5 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(176,196,222);
    glVertex2f(p5,q5);
    for(i = 0; i <= triangle5; i++){
        glVertex2f(p5+(r5*cos(i*tp5/triangle5)), q5+(r5*sin(i*tp5/triangle5)));
    }
    glEnd();

    GLfloat p6 = 0.28f; GLfloat q6 = 0.77f;
    GLfloat r6 = 0.06f;
    int triangle6 = 40;
    GLfloat tp6 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(176,196,222);
    glVertex2f(p6,q6);
    for(i = 0; i <= triangle6; i++){
        glVertex2f(p6+(r6*cos(i*tp6/triangle6)), q6+(r6*sin(i*tp6/triangle6)));
    }
    glEnd();
     glPopMatrix();

    glPushMatrix();
    glTranslatef(position6,0.0f, 0.0f);
    //3rd sky
    GLfloat p7 = -0.25f; GLfloat q7 = 0.90f;
    GLfloat r7 = 0.05f;
    int triangle7 = 40;
    GLfloat tp7 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(176,196,222);
    glVertex2f(p7,q7);
    for(i = 0; i <= triangle7; i++){
        glVertex2f(p7+(r7*cos(i*tp7/triangle7)), q7+(r7*sin(i*tp7/triangle7)));
    }
    glEnd();

    GLfloat p8 = -0.13f; GLfloat q8 = 0.90f;
    GLfloat r8 = 0.10f;
    int triangle8 = 40;
    GLfloat tp8 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(176,196,222);
    glVertex2f(p8,q8);
    for(i = 0; i <= triangle8; i++){
        glVertex2f(p8+(r8*cos(i*tp8/triangle8)), q8+(r8*sin(i*tp8/triangle8)));
    }
    glEnd();

    GLfloat p9 = 0.0f; GLfloat q9 = 0.90f;
    GLfloat r9 = 0.06f;
    int triangle9 = 40;
    GLfloat tp9 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(176,196,222);
    glVertex2f(p9,q9);
    for(i = 0; i <= triangle9; i++){
        glVertex2f(p9+(r9*cos(i*tp9/triangle9)), q9+(r9*sin(i*tp9/triangle9)));
    }
    glEnd();
     glPopMatrix();



   int a;//under stand circle

    GLfloat p100=0.63f; GLfloat q100= -0.83f; GLfloat r100 = 0.08f;
    int tringle100=40;

    GLfloat tp100 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p100,q100);
    for(a= 0;a<=tringle100; a++)
    {
        glVertex2f (
                    p100+(r100*cos(a*tp100/tringle100)),
                    q100+(r100*sin(a*tp100/tringle100))
                    );


    }
    glEnd ();



    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.425f, -0.83f);
    glVertex2f(0.435f, -0.83f);
    glVertex2f(0.435f, -0.65f);
    glVertex2f(0.425f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.39f, -0.78f);
    glVertex2f(0.40f, -0.78f);
    glVertex2f(0.40f, -0.65f);
    glVertex2f(0.39f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.46f, -0.78f);
    glVertex2f(0.47f, -0.78f);
    glVertex2f(0.47f, -0.65f);
    glVertex2f(0.46f, -0.65f);
    glEnd();

    GLfloat p30=0.43f; GLfloat q30= -0.63f; GLfloat r30 = 0.05f;
    int tringle30=40;

    GLfloat tp30 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p30,q30);
    for(a= 0;a<=tringle30; a++)
    {
        glVertex2f (p30+(r30*cos(a*tp30/tringle30)),q30+(r30*sin(a*tp30/tringle30)));
    }
    glEnd ();


    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.825f, -0.83f);
    glVertex2f(0.835f, -0.83f);
    glVertex2f(0.835f, -0.65f);
    glVertex2f(0.825f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.79f, -0.78f);
    glVertex2f(0.80f, -0.78f);
    glVertex2f(0.80f, -0.65f);
    glVertex2f(0.79f, -0.65f);
    glEnd();
    glBegin(GL_QUADS);//toll stand
    glColor3f(0.87f, 0.68f, 0.45f);
    glVertex2f(0.86f, -0.78f);
    glVertex2f(0.87f, -0.78f);
    glVertex2f(0.87f, -0.65f);
    glVertex2f(0.86f, -0.65f);
    glEnd();

    GLfloat p31=0.83f; GLfloat q31= -0.63f; GLfloat r31 = 0.05f;
    int tringle31=40;

    GLfloat tp31 =2.0f * PI  ;

    glBegin (GL_TRIANGLE_FAN);
    glColor3ub ( 160,157,156);
    glVertex2f (p31,q31);
    for(a= 0;a<=tringle31; a++)
    {
        glVertex2f (p31+(r31*cos(a*tp31/tringle31)),q31+(r31*sin(a*tp31/tringle31)));
    }
    glEnd ();


    glBegin(GL_QUADS);//CHAIR
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.35f, -0.15f);
    glVertex2f(0.50f, -0.15f);
    glVertex2f(0.525f, -0.05f);
    glVertex2f(0.375f, -0.05f);
    glEnd();

    glBegin(GL_LINES);
    glColor3f(1.0f, 1.0f, 1.0f);
    glVertex2f(0.36f, -0.15f);
    glVertex2f(0.36f, -0.225f);

    glVertex2f(0.49f, -0.15f);
    glVertex2f(0.49f, -0.225f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, -0.145f);

    glVertex2f(0.52f, -0.05f);
    glVertex2f(0.52f, 0.05f);

    glVertex2f(0.38f, -0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.05f);
    glVertex2f(0.38f, 0.05f);

    glVertex2f(0.52f, 0.025f);
    glVertex2f(0.38f, 0.025f);

    glVertex2f(0.52f, 0.00f);
    glVertex2f(0.38f, 0.00f);

    glEnd();



    glBegin(GL_QUADS);//table stand
    glColor3ub(112,128,144);
    glVertex2f(0.62f, -0.81f);
    glVertex2f(0.64f, -0.81f);
    glColor3ub(119,136,153);
    glVertex2f(0.64f, -0.64f);
    glVertex2f(0.62f, -0.64f);
    glEnd();

         int j;//bigger circle of foyara
    GLfloat p11=0.63f; GLfloat q11= -0.52f; GLfloat r11 = 0.132f;
    int tringle11=40;

    GLfloat tp11 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3f ( 0.46f,0.46f,0.46f);//grey color
    glVertex2f (p11,q11);
    for(j= 0;j<=tringle11; j++)
    {
    glVertex2f (
    p11+(r11*cos(j*tp11/tringle11)),
    q11+(r11*sin(j*tp11/tringle11))
    );
    }
    glEnd ();



    int k;//small circle foyara
    GLfloat p12=0.63f; GLfloat q12= -0.50f; GLfloat r12 = 0.115f;
    int tringle12=40;

    GLfloat tp12 =10.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub (188,143,143);//blue color
    glVertex2f (p12,q12);
    for(k= 0;k<=tringle12; k++)
    {
    glVertex2f (
    p12+(r12*cos(k*tp12/tringle12)),
    q12+(r12*sin(k*tp12/tringle12))
    );
    }
    glEnd ();


    //left mic
    GLfloat p13 = -0.75f; GLfloat q13 = 0.375f;
    GLfloat r13 = 0.04f;
    int triangle13 = 40;
    GLfloat tp13 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p13,q13);
    for(i = 0; i <= triangle13; i++){
        glVertex2f(p13+(r13*cos(i*tp13/triangle13)), q13+(r13*sin(i*tp13/triangle13)));
    }
    glEnd();

    GLfloat p14 = -0.75f; GLfloat q14 = 0.375f;
    GLfloat r14 = 0.01f;
    int triangle14 = 40;
    GLfloat tp14 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p14,q14);
    for(i = 0; i <= triangle14; i++){
        glVertex2f(p14+(r14*cos(i*tp14/triangle14)), q14+(r14*sin(i*tp14/triangle14)));
    }
    glEnd();

    //right mic
    GLfloat p15 = 0.2f; GLfloat q15 = 0.375f;
    GLfloat r15 = 0.04f;
    int triangle15 = 40;
    GLfloat tp15 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(48, 113, 209);
    glVertex2f(p15,q15);
    for(i = 0; i <= triangle15; i++){
        glVertex2f(p15+(r15*cos(i*tp15/triangle15)), q15+(r15*sin(i*tp15/triangle15)));
    }
    glEnd();

    GLfloat p16 = 0.2f; GLfloat q16 = 0.375f;
    GLfloat r16 = 0.01f;
    int triangle16 = 40;
    GLfloat tp16 = 2.0f*PI;
    glBegin(GL_TRIANGLE_FAN);
    glColor3ub(74, 102, 143);
    glVertex2f(p16,q16);
    for(i = 0; i <= triangle16; i++){
        glVertex2f(p16+(r16*cos(i*tp16/triangle16)), q16+(r16*sin(i*tp16/triangle16)));
    }
    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.275f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.30f, 0.10f);
    glVertex2f(0.30f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.28f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.28f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.32f, 0.30f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.30f, 0.32f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.31f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.29f, 0.29f);

    glVertex2f(0.30f, 0.30f);
    glVertex2f(0.31f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.095f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.105f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.30f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.350f, 0.17f);

    glVertex2f(0.325f, 0.05f);
    glVertex2f(0.325f, 0.25f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.305f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.23f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.345f, 0.25f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.325f, 0.27f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.26f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.315f, 0.24f);

    glVertex2f(0.325f, 0.25f);
    glVertex2f(0.335f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.145f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.325f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.375f, 0.22f);

    glVertex2f(0.350f, 0.10f);
    glVertex2f(0.350f, 0.30f);

    //flower
    glColor3ub(153, 9, 107);
    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.33f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.28f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.370f, 0.30f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.350f, 0.32f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.31f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.340f, 0.29f);

    glVertex2f(0.350f, 0.30f);
    glVertex2f(0.360f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.875f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.90f, 0.10f);
    glVertex2f(0.90f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.88f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.28f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.92f, 0.30f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.90f, 0.32f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.31f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.89f, 0.29f);

    glVertex2f(0.90f, 0.30f);
    glVertex2f(0.91f, 0.29f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.095f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.105f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.90f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.950f, 0.17f);

    glVertex2f(0.925f, 0.05f);
    glVertex2f(0.925f, 0.25f);

    //flower
    glColor3ub(235, 235, 111);
    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.905f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.23f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.945f, 0.25f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.925f, 0.27f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.26f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.915f, 0.24f);

    glVertex2f(0.925f, 0.25f);
    glVertex2f(0.935f, 0.24f);

    glEnd();

    glBegin(GL_LINES);//tree besides river
    glColor3ub(32, 110, 16);
    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.145f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.925f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.975f, 0.22f);

    glVertex2f(0.950f, 0.10f);
    glVertex2f(0.950f, 0.30f);

    //flower
    glColor3ub(235, 40, 111);
    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.93f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.28f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.970f, 0.30f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.950f, 0.32f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.31f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.940f, 0.29f);

    glVertex2f(0.950f, 0.30f);
    glVertex2f(0.960f, 0.29f);

    glEnd();


    int l;//left minar gombuj

    GLfloat p17=-0.751f; GLfloat q17= 0.55f; GLfloat r17= 0.07f;
    int tringle17=40;

    GLfloat tp17 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (p17,q17);
    for(l= 0;l<=tringle17; l++)
    {
    glVertex2f (
    p17+(r17*cos(l*tp17/tringle17)),
    q17+(r17*sin(l*tp17/tringle17))
    );
    }
    glEnd ();

    int s;// Right minar gombuj

    GLfloat x18=0.2f; GLfloat h18= 0.55f; GLfloat Gr18 = 0.07f;
    int Gtringle18=40;

    GLfloat tp18 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x18,h18);
    for(s= 0;s<=Gtringle18; s++)
    {
    glVertex2f (
    x18+(Gr18*cos(s*tp18/Gtringle18)),
    h18+(Gr18*sin(s*tp18/Gtringle18))
    );
    }
    glEnd ();

    //2nd left gombuj
    GLfloat x1=-0.604f; GLfloat h1= 0.515f; GLfloat ra = 0.07f;
    int Gtringle=40;

    GLfloat tp19 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x1,h1);
    for(l= 0;l<=Gtringle; l++)
    {
    glVertex2f (
    x1+(ra*cos(l*tp19/Gtringle)),
    h1+(ra*sin(l*tp19/Gtringle))
    );
    }
    glEnd ();

    int m;// 3rd left gombuj

    GLfloat x2=-0.460f; GLfloat h2= 0.515f; GLfloat Gr = 0.07f;
    int Gtringle1=40;

    GLfloat tp20 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x2,h2);
    for(m= 0;m<=Gtringle1; m++)
    {
    glVertex2f (
    x2+(Gr*cos(m*tp20/Gtringle1)),
    h2+(Gr*sin(m*tp20/Gtringle1))
    );
    }
    glEnd ();

    int n;// 4th left gombuj

    GLfloat x3=-0.315f; GLfloat h3= 0.515f; GLfloat Gr1 = 0.07f;
    int Gtringle21=40;

    GLfloat tp21 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x3,h3);
    for(n= 0;n<=Gtringle21; n++)
    {
    glVertex2f (
        x3+(Gr1*cos(n*tp21/Gtringle21)),
        h3+(Gr1*sin(n*tp21/Gtringle21))
    );
    }
    glEnd ();

    int p;// 5th left gombuj

    GLfloat x4=-0.170f; GLfloat h4= 0.515f; GLfloat Gr2 = 0.07f;
    int Gtringle22=40;

    GLfloat tp22=1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x4,h4);
    for(p= 0;p<=Gtringle22; p++)
    {
    glVertex2f (
    x4+(Gr2*cos(p*tp22/Gtringle22)),
    h4+(Gr2*sin(p*tp22/Gtringle22))
    );
    }
    glEnd ();

    int q;// 6th left gombuj

    GLfloat x5=-0.028f; GLfloat h5= 0.515f; GLfloat Gr3 = 0.07f;
    int Gtringle23=40;

    GLfloat tp23 =1.0f * PI ;
    glBegin (GL_TRIANGLE_FAN);
    glColor3ub(161, 82, 26);
    glVertex2f (x5,h5);
    for(q= 0;q<=Gtringle23; q++)
    {
    glVertex2f (
    x5+(Gr3*cos(q*tp23/Gtringle23)),
    h5+(Gr3*sin(q*tp23/Gtringle23))
    );
    }
    glEnd ();

    glBegin(GL_TRIANGLES);//right side  small tree triangle-1
   glColor3ub(47, 92, 13);
    glVertex2f(0.00f, -0.35f);//middle line
    glVertex2f(0.10f, -0.35f);
    glVertex2f(0.05f, -0.15f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-2
   glColor3ub(47, 92, 13);
    glVertex2f(0.05f, -0.25f);//middle line
    glVertex2f(0.10f, -0.4f);
    glVertex2f(0.00f, -0.4f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  small tree triangle-3
   glColor3ub(47, 92, 13);
    glVertex2f(0.05f, -0.30f);//middle line
    glVertex2f(0.10f, -0.45f);
    glVertex2f(0.00f, -0.45f);

    glEnd();



    glBegin(GL_TRIANGLES);//left side  small tree triangle-1
    glColor3ub(47, 92, 13);
    glVertex2f(-0.55f, -0.35f);//middle line
    glVertex2f(-0.65f, -0.35f);
    glVertex2f(-0.60f, -0.15f);

    glEnd();
        glBegin(GL_TRIANGLES);//left side  small tree triangle-2
 glColor3ub(47, 92, 13);
    glVertex2f(-0.55f, -0.4f);//middle line
    glVertex2f(-0.65f, -0.4f);
    glVertex2f(-0.60f, -0.25f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  small tree triangle-3
glColor3ub(47, 92, 13);
    glVertex2f(-0.55f, -0.45f);//middle line
    glVertex2f(-0.65f, -0.45f);
    glVertex2f(-0.60f, -0.30f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side  middle tree triangle-1
glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -0.40f);//middle line
    glVertex2f(0.1f, -0.6f);
    glVertex2f(0.2f, -0.6f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-2
    glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -0.50f);//middle line
    glVertex2f(0.1f, -0.65f);
    glVertex2f(0.2f, -0.65f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  middle tree triangle-3
glColor3ub(47, 92, 13);
    glVertex2f(0.15f, -0.55f);//middle line
    glVertex2f(0.1f, -0.7f);
    glVertex2f(0.2f, -0.7f);
    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -0.40f);//middle line
    glVertex2f(-0.75f, -0.60f);
    glVertex2f(-0.65f, -0.60f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  middle tree triangle-1
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -0.50f);//middle line
    glVertex2f(-0.75f, -0.65f);
    glVertex2f(-0.65f, -0.65f);

    glEnd();


    glBegin(GL_TRIANGLES);//left side  middle tree triangle-2
    glColor3ub(47, 92, 13);
    glVertex2f(-0.70f, -0.55f);//middle line
    glVertex2f(-0.75f, -0.70f);
    glVertex2f(-0.65f, -0.70f);

    glEnd();

    glBegin(GL_TRIANGLES);//right side last tree triangle-1
   glColor3ub(47, 92, 13);
    glVertex2f(0.25f, -0.65f);//middle line
    glVertex2f(0.2f, -0.85f);
    glVertex2f(0.3f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side last tree triangle-2
    glColor3ub(47, 92, 13);
    glVertex2f(0.25f, -0.75f);//middle line
    glVertex2f(0.2f, -0.90f);
    glVertex2f(0.3f, -0.90f);

    glEnd();
    glBegin(GL_TRIANGLES);//right side  last tree triangle-3
    glColor3ub(47, 92, 13);
    glVertex2f(0.25f, -0.80f);//middle line
    glVertex2f(0.2f, -0.95f);
    glVertex2f(0.3f, -0.95f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-1
   glColor3ub(47, 92, 13);
    glVertex2f(-0.80f, -0.65f);//middle line
    glVertex2f(-0.85f, -0.85f);
    glVertex2f(-0.75f, -0.85f);

    glEnd();
    glBegin(GL_TRIANGLES);//left side  last tree triangle-2
   glColor3ub(47, 92, 13);
    glVertex2f(-0.80f, -0.75f);//middle line
    glVertex2f(-0.85f, -0.9f);
    glVertex2f(-0.75f, -0.9f);

    glEnd();

    glBegin(GL_TRIANGLES);//left side  last tree triangle-3
    glColor3ub(47, 92, 13);
    glVertex2f(-0.80f, -0.8f);//middle line
    glVertex2f(-0.85f, -0.95f);
    glVertex2f(-0.75f, -0.95f);

    glEnd();


GLfloat p26=-0.95f; GLfloat q26= 0.7f; GLfloat r26 = 0.15f;// big tree
int tringle26=40;
GLfloat tp26 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub(47, 92, 13);
glVertex2f (p26,q26);
for(i= 0;i<=tringle26; i++)
{
glVertex2f(p26+(r26*cos(i*tp26/tringle26)),q26+(r26*sin(i*tp26/tringle26)));
}
glEnd ();


//int j; //Right Leaf
GLfloat p27=-0.75f; GLfloat q27= 0.7f; GLfloat r27 = 0.15f;
int tringle27=40;
GLfloat tp27 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub(47, 92, 13);
glVertex2f (p27,q27);
for(i= 0;i<=tringle27; i++)
{
glVertex2f (p27+(r27*cos(i*tp27/tringle27)),q27+(r27*sin(i*tp27/tringle27)));
}
glEnd ();


//int k; //Upper Leaf
GLfloat p28=-0.85f; GLfloat q28= 0.85f; GLfloat r28 = 0.15f;
int tringle28=40;
GLfloat tp28 =11.0f * PI ;
glBegin (GL_TRIANGLE_FAN);
glColor3ub(47, 92, 13);
glVertex2f (p28,q28);
for(i= 0;i<=tringle28; i++)
{
glVertex2f (
p28+(r28*cos(i*tp28/tringle28)),q28+(r28*sin(i*tp28/tringle28)));
}
glEnd ();


glBegin(GL_QUADS);//Tree ground
glColor3ub(58,24,16);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.85f, -0.2f);
glVertex2f(-0.95f, -0.2f);
glEnd();


glBegin(GL_QUADS); //Left Tributariness
glColor3ub(58,24,16);
glVertex2f(-0.95f, 0.5f);
glVertex2f(-1.0f, 0.6f);
glVertex2f(-0.95f, 0.6f);
glVertex2f(-0.85f, 0.5f);
glEnd();


glBegin(GL_QUADS); //Right Tributariness
glColor3ub(58,24,16);
glVertex2f(-0.85f, 0.5f);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.95f, 0.5f);
glEnd();


glBegin(GL_POLYGON); //Right Tributariness
glColor3ub(58,24,16);
glVertex2f(-0.85f, 0.6f);
glVertex2f(-0.82f, 0.8f);
glVertex2f(-0.79f, 0.8f);
glVertex2f(-0.8f, 0.6f);
glEnd();


glBegin(GL_QUADS);
glColor3ub(58,24,16);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();


    glBegin(GL_QUADS);
glColor3ub(58,24,16);
glVertex2f(-0.8f, 0.6f);
glVertex2f(-0.76f, 0.7f);
glVertex2f(-0.76f, 0.75f);
glVertex2f(-0.8f, 0.7f);
glEnd();

/*glBegin(GL_POLYGON);// TREE BOTTOM SURFACE
glColor3f(0.55f, 0.27f, 0.0f);
glVertex2f(-1.0f, -0.15f);
glVertex2f(-1.0f, -0.3f);
glVertex2f(-0.95f, -0.25f);
glVertex2f(-0.95f, -0.20f);
glVertex2f(-0.85f, -0.20f);
glVertex2f(-0.85f, -0.25f);
glVertex2f(-0.80f, -0.30f);
glVertex2f(-0.80f, -0.15f);
glEnd();*/

glBegin(GL_QUADS);//road
glColor3ub(64,33,5);
//glColor3f(0.58f, 0.23f, 0.19f);
glVertex2f(-0.47f, -0.25f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(0.15f, -1.0f);
glVertex2f(-0.65f, -1.0f);

glEnd();

glBegin(GL_LINES);//road
glColor3ub(89,38,11);
glVertex2f(-0.475f, -0.30f);
glVertex2f(-0.0875f, -0.30f);
glVertex2f(-0.490f, -0.35f);
glVertex2f(-0.07f, -0.35f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.515f, -0.45f);
glVertex2f(-0.035f, -0.45f);
glVertex2f(-0.525f, -0.5f);
glVertex2f(-0.02f, -0.5f);
glVertex2f(-0.54f, -0.55f);
glVertex2f(0.00f, -0.55f);
glVertex2f(-0.554f, -0.6f);
glVertex2f(0.017f, -0.6f);
glVertex2f(-0.562f, -0.65f);
glVertex2f(0.0355f, -0.65f);
glVertex2f(-0.575f, -0.7f);
glVertex2f(0.052f, -0.7f);
glVertex2f(-0.587f, -0.75f);
glVertex2f(0.064f, -0.75f);
glVertex2f(-0.6f, -0.8f);
glVertex2f(0.084f, -0.8f);
glVertex2f(-0.615f, -0.85f);
glVertex2f(0.1f, -0.85f);
glVertex2f(-0.625f, -0.9f);
glVertex2f(0.12f, -0.9f);
glVertex2f(-0.64f, -0.95f);
glVertex2f(0.135f, -0.95f);
glVertex2f(-0.60f, -0.8f);
glVertex2f(-0.60f, -1.0f);
glVertex2f(-0.55f, -0.6f);
glVertex2f(-0.55f, -1.0f);
glVertex2f(-0.5f, -0.4f);
glVertex2f(-0.5f, -1.0f);
glVertex2f(-0.45f, -0.25f);
glVertex2f(-0.45f, -1.0f);
glVertex2f(-0.4f, -0.25f);
glVertex2f(-0.4f, -1.0f);
glVertex2f(-0.35f, -0.25f);
glVertex2f(-0.35f, -1.0f);
glVertex2f(-0.3f, -0.25f);
glVertex2f(-0.3f, -1.0f);
glVertex2f(-0.25f, -0.25f);
glVertex2f(-0.25f, -1.0f);
glVertex2f(-0.2f, -0.25f);
glVertex2f(-0.2f, -1.0f);
glVertex2f(-0.15f, -0.25f);
glVertex2f(-0.15f, -1.0f);
glVertex2f(-0.1f, -0.25f);
glVertex2f(-0.1f, -1.0f);
glVertex2f(-0.05f, -0.4f);
glVertex2f(-0.05f, -1.0f);
glVertex2f(0.0f, -0.55f);
glVertex2f(0.0f, -1.0f);
glVertex2f(0.05f, -0.7f);
glVertex2f(0.05f, -1.0f);
glVertex2f(0.10f, -0.85f);
glVertex2f(0.10f, -1.0f);
glEnd();


    glBegin(GL_QUADS);//ghatpar
    glColor3f(0.21f, 0.16f, 0.10f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.85f, 0.15f);
    glVertex2f(0.75f, 0.15f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.70f, 0.0f);
    glVertex2f(0.71f, 0.0f);
    glVertex2f(0.71f, 0.08f);
    glVertex2f(0.70f, 0.08f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.72f, 0.060f);
    glVertex2f(0.73f, 0.060f);
    glVertex2f(0.73f, 0.12f);
    glVertex2f(0.72f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar left upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.74f, 0.12f);
    glVertex2f(0.75f, 0.12f);
    glVertex2f(0.75f, 0.18f);
    glVertex2f(0.74f, 0.18f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right lower  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.890f, 0.0f);
    glVertex2f(0.90f, 0.0f);
    glVertex2f(0.9f, 0.08f);
    glVertex2f(0.890f, 0.08f);
    glEnd();

    glBegin(GL_QUADS);//ghatpar right middle  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.87f, 0.060f);
    glVertex2f(0.88f, 0.060f);
    glVertex2f(0.88f, 0.12f);
    glVertex2f(0.87f, 0.12f);
    glEnd();
    glBegin(GL_QUADS);//ghatpar right upper  khuti
    glColor3f(0.21f, 0.20f, 0.08f);
    glVertex2f(0.85f, 0.12f);
    glVertex2f(0.86f, 0.12f);
    glVertex2f(0.86f, 0.18f);
    glVertex2f(0.85f, 0.18f);
    glEnd();


  glFlush();
}




    /*void handleMouse(int button, int state, int x, int y) {
    if (button == GLUT_LEFT_BUTTON)
    {	speed += 0.1f;
    }
    if (button == GLUT_RIGHT_BUTTON)
    {speed -= 0.1f;   }
    glutPostRedisplay();}*/


    void handleKeypress(unsigned char key, int x, int y) {
    switch (key) {
    case 'e':
    night();
    glutDisplayFunc(night);
    break;
    case 'f':
    fajr();
    glutDisplayFunc(fajr);
    break;

    case 'a':
    asr();
    glutDisplayFunc(asr);
    break;
    case 'm':
    maghrib();
    glutDisplayFunc(maghrib);
    break;
    case 'j':
    display();
    glutDisplayFunc(display);
    break;


    glutPostRedisplay();
    }
    }


























int main(int argc, char** argv){
    glutInit(&argc, argv);
    glutInitWindowSize(520, 520);
    glutInitWindowPosition(400, 200);
    glutCreateWindow("Shat Ghambhuj Masjid");
    glutDisplayFunc(display);
    glutTimerFunc(100, update, 0);
    glutTimerFunc(100, update1, 0);
    //glutTimerFunc(100, update2, 0);
    glutTimerFunc(100, update3, 0);
    glutTimerFunc(100, update4, 0);
    glutTimerFunc(100, update5, 0);
    glutTimerFunc(100, update6, 0);


    glutKeyboardFunc(handleKeypress);
    sndPlaySound("azan1.mp3",SND_ASYNC);
    //glutMouseFunc(handleMouse);
    glutMainLoop();
    return 0;
}





























































