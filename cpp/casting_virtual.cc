// Author: mmuthanna@google.com (Mohit Cheppudira)


#include <stdio.h>
class Object{
    public:
        virtual int Hash() {return (int)this;}
};
class A:virtual public Object{
    public:
        int x;
        A(){x=1;}
};
class B:virtual public Object{
     public:
         int y;
         B(){y=2;}
};
class C:public A,public B{
     public:
         int z;
         C() {z=3;}
};

void blah (Object * reallyC) {
    C* cP = dynamic_cast<C*>(reallyC);
    printf ("cP.x: %d cP.y: %d\n",cP->x,cP->y);
}

int main () {
   C * c= new C;
   A * aP=c;
   B * bP=c;
   printf ("aP.x %d bP.y: %d\n",aP->x ,bP->y);
   {
     C * cP=(C*)(void*)aP;
     printf ("cP.x: %d cP.y: %d\n",cP->x,cP->y);
   }
   C * cP=(C*)(void*)bP;
   printf ("cP.x: %d cP.y: %d\n",cP->x,cP->y);
   C * cPgood= (C*)bP;
   printf ("dyncP.x: %d dyncP.y: %d\n",cPgood->x, cPgood->y);

   printf("%d\n", aP->Hash());
   printf("%d\n", bP->Hash());
   printf("%d\n", c->A::Hash());
   printf("%d\n", c->B::Hash());

   return 0;
}
