import sys

from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *

from bulletphysics import *

class Scene(object):

    def Diaplay(self):
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);


        self.world.stepSimulation(self.time_step/600.0, 10)
        self.box_motionstate.getWorldTransform(self.trans)
        origin = self.trans.getOrigin()
        rot = self.trans.getRotation()
        print origin.getX(), origin.getY(), origin.getZ()
        #print rot.getW(), rot.getX(), rot.getY(), rot.getZ()

        print(dir(self.box_motionstate))
        self.time_step += 1

        glLoadIdentity()
        glTranslatef(0.0, 10.0, -40.0);

        glRotatef(30.0, 1.0, 1.0, 0.0)
        # glRotatef(30.0, 0.0, 1.0, 0.0)
        # glRotatef(30.0, 1.0, 0.0, 0.0)

        glBegin(GL_QUADS);          # Start Drawing The Cube

        Y = origin.getY()
        #Y = 1.0
        glColor3f(1.0,0.0,0.0);         # Set The Color To Red
        glVertex3f( 1.0, Y, 1.0);     # Top Right Of The Quad (Front)
        glVertex3f(-1.0, Y, 1.0);     # Top Left Of The Quad (Front)
        glVertex3f(-1.0,Y-2.0, 1.0);     # Bottom Left Of The Quad (Front)
        glVertex3f( 1.0,Y-2.0, 1.0);     # Bottom Right Of The Quad (Front)

        glEnd();


        glutSwapBuffers()

    def Idle(self):
        glutPostRedisplay()

    def InitGL(self, width, height):

        glClearColor(0.0, 0.0, 0.0, 0.0)
        glClearDepth(1.0)
        glDepthFunc(GL_LESS)
        glEnable(GL_DEPTH_TEST)
        glShadeModel(GL_SMOOTH)
        
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        gluPerspective(45.0, float(width)/float(height), 0.1, 100.0)

        glMatrixMode(GL_MODELVIEW)

    def InitBullet(self):
        self.broadphase = DbvtBroadphase()
        self.collisionConfiguration = DefaultCollisionConfiguration()
        self.dispatcher = CollisionDispatcher(self.collisionConfiguration)

        self.solver = SequentialImpulseConstraintSolver()

        self.world = DiscreteDynamicsWorld(
            self.dispatcher, self.broadphase,
            self.solver, self.collisionConfiguration)

        self.world.setGravity( Vector3(0, -1, 0) )

        self.ground = StaticPlaneShape(Vector3(0,1,0),1)
        self.box = BoxShape( Vector3(1, 1, 1));

        self.ground_motionstate = DefaultMotionState(
            Transform(Quaternion(0,0,0,1),
                Vector3(0,-10,0)) )

        self.ground_rigidbody_info = RigidBodyConstructionInfo(
            0, self.ground_motionstate, self.ground, Vector3(0,0,0))

        self.ground_rigidbody = RigidBody(self.ground_rigidbody_info)

        self.world.addRigidBody(self.ground_rigidbody)

        self.box_motionstate = DefaultMotionState(
            Transform(Quaternion(0,0,0,1), Vector3(0,0,0)) )

        box_mass = 1 

        local_inertia = Vector3(0,0,0)

        self.box.calculateLocalInertia(box_mass, local_inertia)

        self.box_rigidbody_info = RigidBodyConstructionInfo(
            box_mass, self.box_motionstate,
            self.box, local_inertia)

        self.box_rigidbody = RigidBody(self.box_rigidbody_info)

        self.world.addRigidBody(self.box_rigidbody)

        self.box_rigidbody.applyForce(
            Vector3(0.0, 0.0, 10.0),
            Vector3(0.0, 5.0, 0.0) )

        self.trans = Transform()
        self.time_step = 1

def main():
    glutInit(sys.argv)
    glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH)

    glutInitWindowSize(640, 480)
    glutInitWindowPosition(0, 0)
    window = glutCreateWindow("Cube")

    scene = Scene()
    scene.InitGL(640, 480)
    scene.InitBullet()

    glutDisplayFunc(scene.Diaplay)
    glutIdleFunc(scene.Idle)

    glutMainLoop()


if __name__ == '__main__':
    main()