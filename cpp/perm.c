//Permutation of any given word
#include<stdio.h>
#include<conio.h>
#include<string.h>
void bubblesort(char a[],int n)
{
 int i,j;
 char temp;
 for(i=0;i<n;i++)
 {
  for(j=0;j<n-i-1;j++)
  {
   if(a[j]>a[j+1])
   {
    temp=a[j];
    a[j]=a[j+1];
    a[j+1]=temp;
   }
  }
 }
 return;
}
factorial(int j)
{
 int i,factorial=1;
 for(i=1;i<=j;i++)
 {
  factorial=factorial*i;
 }
 return(factorial);
}
char a[10];
void main()
{
int o,n,i,j=0,k,right,left,recursion=0,product=1,count[4]={0};
char temp;
clrscr();
printf("Enter any word\n");
gets(a);
n=strlen(a);
/*Bubblesort the entire word*/
bubblesort(a,n);
/*Count the number of recursions of each character*/
for(i=0;i<n;i++)
  {
   if(a[i]==a[i+1])
   {
    recursion++;
    count[j]++;
    if(a[i+1]!=a[i+2])
    {
     j++;
    }
   }
  }
if(recursion==0)
{
  o=factorial(n);
}
/*product of factorial of each recursion*/
else
{
  for(i=0;i<(n/2);i++)
  {
     product=product*factorial(count[i]+1);
  }
  o=(factorial(n)/product);
}
for(j=0;j<o;j++)
 {
  printf("%s\n",a);
  k=n-1;
/*Take the descending order string of largest length starting from 
the right end of the array*/
  while(k>0&&a[k]>=a[k+1])
  {
   k--;
  }
  left=k+1;
  right=n-1;
/* Sorting the descending string in ascending order*/
  while(left<=right)
  {
   temp=a[left];
   a[left++]=a[right];
   a[right--]=temp;
  }
  i=k+1;
/*Comparing it with the element before the first element of the smaller
string extracted*/
  while(a[k]>=a[i])
  i++;
/*Swapping the first element larger than a[i], in the smaller substring 
arranged in ascending order ,with a[i]*/ 
  temp=a[k];
  a[k]=a[i];
  a[i]=temp;
  if(j%44==0)
  getch();
 }
 printf("\nNo of permutations : %d",o);
 getch();
}
