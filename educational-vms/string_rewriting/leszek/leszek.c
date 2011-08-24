#include<stdio.h>
#include<string.h>

#define src_max_length 65536
#define val_max_length 1024
#define integer int

// implemented operators:
// \C\dn. \L\dn.
// \Txy \Gn.x1x2...xn \Axy \Dxy
// \Exab
// \I \M \Ox
// \N
// \+m.n. \-m.n. \*m.m. \/m.n. \%m.n.
// \=xy
// \&xy \|xy \!x

int debug=0;
FILE *deb;

void wczytaj(char *s,FILE *plik)
{
 long i;
 for(i=0;!feof(plik);i++)
  s[i]=getc(plik);
 s[i-1]=0;
}

int szukaj_C(const char *s,char znak,long n,char *wynik)
{
 long i,st=-1,ile=-1;
 int b=0;
 if(n==0) st=0;
 for(i=0;s[i]!=0;i++)
 {
   if(s[i]=='\\')
    i++;
   else if(s[i]==znak)
   {
    b++;
    if(b==n) st=i+1;
    if(b==n+1)
    {
     ile=i-st;
     break;
    }
   }
 }
 if(st!=-1)
 {
  if(ile==-1) strcpy(wynik,s+st);
  else {strncpy(wynik,s+st,ile);wynik[ile]=0;}
 }
 else wynik[0]=0;
 return strlen(wynik);
}

int szukaj_O(const char *s)
{
 long i;
 int jest=0;
 for(i=0;s[i]!=0;i++)
  if(s[i]=='\\')
  {
   i++;
   if(s[i]=='O')
   {
    jest=1;
    break;
   }
  }
 return jest;
}

void eval(const char *s,char *s1,long *i,int akt,int out);

integer getnum(const char *s,long *i)
{
 char tmp[val_max_length]; tmp[0]=0;
 integer n=0;
 int j,ev=0;
 if(s[*i]=='\\')
 {
  eval(s,tmp,i,1,0); ev=1;
 }
 else
  strncpy(tmp,s+(*i),val_max_length);
 for(j=0;tmp[j]>='0' && tmp[j]<='9';j++)
  n=n*10+tmp[j]-'0';
 if(!ev) (*i)+=j;
 if(tmp[j]!='.')
  n=-1;
 else
  if(!ev) (*i)++;
 return n;
}

void push(char *s,char c)
{
 char a=c,m;
 int i;
 for(i=0;s[i]!=0;i++)
 {
  m=s[i];
  s[i]=a;
  a=m;
 }
 s[i]=a; s[i+1]=0;
}

void int2str(integer n,char *s)
{
 s[0]='.'; s[1]=0;
 if(n<0) s[0]=0;
 else
  for(;n>0;n/=10)
   push(s,48+n%10);
}

void zrob(const char *s,char *s2)
{
 strcpy(s2,s);
 while(strchr(s2,'\n')!=NULL)
  *strchr(s2,'\n')='^';
}

void eval(const char *s,char *s1,long *i,int akt,int out)
{
 integer m,n,j;
 char znak;
 char tmp[val_max_length]; tmp[0]=0;
 char tmp2[val_max_length]; tmp2[0]=0;
 char tmp3[val_max_length]; tmp3[0]=0;
 if(s[*i]==0) return;
 if(s[*i]!='\\')
 {
  tmp[0]=s[*i];tmp[1]=0;
  (*i)++;
 }
 else
 {
  (*i)++;
  (*i)++;
  switch(s[*i-1])
  {
   case 'C':    
    if(s[*i]!='\\') break;
    (*i)++;
    if(s[*i]=='\\') break;
    znak=s[*i];
    (*i)++;
    n=getnum(s,i);
    if(n!=-1)
     szukaj_C(s,znak,n,tmp);
    if(debug) {zrob(tmp,tmp2);fprintf(deb," C %c %d %s\n",znak,n,tmp2);}
    break;
   case 'L':
    if(s[*i]!='\\') break;
    (*i)++;
    if(s[*i]=='\\') break;
    znak=s[*i];
    (*i)++;
    n=getnum(s,i);
    if(n!=-1)
     int2str(szukaj_C(s,znak,n,tmp2),tmp);
    if(debug) {zrob(tmp,tmp2);fprintf(deb," L %c %d %s\n",znak,n,tmp2);}
    break;
   case 'T':
    eval(s,tmp,i,akt,out);
    eval(s,tmp,i,akt,out);
    if(debug) {zrob(tmp,tmp2);fprintf(deb," T %s\n",tmp2);}
    break;
   case 'G':
    n=getnum(s,i);
    if(n!=-1)
     for(j=0;j<n;j++)
      eval(s,tmp,i,akt,out);
    if(debug) {zrob(tmp,tmp2);fprintf(deb," G %d %s\n",n,tmp2);}
    break;
   case 'E':
    eval(s,tmp2,i,akt,0);
    if(strcmp(tmp2,"")!=0)
    {
     eval(s,tmp,i,akt,out);
     eval(s,tmp2,i,0,0);
    }
    else
    {
     eval(s,tmp2,i,0,0);
     eval(s,tmp,i,akt,out);
    }
    if(debug) {zrob(tmp,tmp2);fprintf(deb," E %s\n",tmp2);}
    break;
   case 'I':
    if(akt)
    {
     tmp[0]=getchar();tmp[1]=0;
     if(tmp[0]=='\\') {tmp[1]='\\';tmp[2]=0;}
    }
    if(debug) {zrob(tmp,tmp2);fprintf(deb," I %s\n",tmp2);}
    break;
   case 'M':
    if(akt)
    {
     j=0;
     while((znak=getchar())=='0') ;
     tmp[0]=znak;
     while(tmp[j]>='0' && tmp[j]<='9')
     {
      j++;
      tmp[j]=getchar();
     }
     if(tmp[j]=='\n')
      tmp[j]='.';tmp[j+1]=0;
     else
      tmp[0]=0;
    }
    if(debug) {zrob(tmp,tmp2);fprintf(deb," M %s\n",tmp2);}
    break;
   case 'O':
    eval(s,tmp2,i,akt,1);
    if(akt)
     fputs(tmp2,stdout);
    if(debug) {zrob(tmp2,tmp3);fprintf(deb," O %s %d\n",tmp3,akt);}
    break;
   case 'N':
    if(debug) fprintf(deb," N\n");
    break;
   case 'D':
    eval(s,tmp2,i,0,0);
    eval(s,tmp,i,akt,out);
    if(debug) {zrob(tmp,tmp2);fprintf(deb," D %s\n",tmp2);}
    break;
   case 'A':
    eval(s,tmp,i,akt,out);
    eval(s,tmp2,i,0,0);
    if(debug) {zrob(tmp,tmp2);fprintf(deb," A %s\n",tmp2);}
    break; // \Wxy? \L\dn.? \Gn.<expr1>..<exprn>?
   case '+':
    m=getnum(s,i);
    n=getnum(s,i);
    if((m!=-1) && (n!=-1))
     int2str(m+n,tmp);
    if(debug) fprintf(deb," + %d %d\n",m,n);
    break;
   case '-':
    m=getnum(s,i);
    n=getnum(s,i);
    if((m!=-1) && (n!=-1))
    {
     if(m>n)
      int2str(m-n,tmp);
     else
      strcpy(tmp,".");
    }
    if(debug) fprintf(deb," - %d %d\n",m,n);
    break;
   case '*':
    m=getnum(s,i);
    n=getnum(s,i);
    if((m!=-1) && (n!=-1))
     int2str(m*n,tmp);
    if(debug) fprintf(deb," * %d %d\n",m,n);
    break;
   case '/':
    m=getnum(s,i);
    n=getnum(s,i);
    if((m!=-1) && (n!=-1) && (n!=0))
     int2str(m/n,tmp);
    if(debug) fprintf(deb," / %d %d\n",m,n);
    break;
   case '%':
    m=getnum(s,i);
    n=getnum(s,i);
    if((m!=-1) && (n!=-1) && (n!=0)) 
     int2str(m%n,tmp);
    if(debug) fprintf(deb," \% %d %d\n",m,n); 
    break; 
   case '=':
    eval(s,tmp2,i,akt,0);
    eval(s,tmp3,i,akt,0);
    if(strcmp(tmp2,tmp3)==0)
     strcpy(tmp,"1");
    if(debug) fprintf(deb," = %s %s\n",tmp2,tmp3);
    break;
   case '&':
    eval(s,tmp2,i,akt,0);
    eval(s,tmp3,i,akt,0);
    if((strcmp(tmp2,"")!=0)&&(strcmp(tmp3,"")!=0))
     strcpy(tmp,"1");
    break;
   case '|':
    eval(s,tmp2,i,akt,0);
    eval(s,tmp3,i,akt,0);
    if((strcmp(tmp2,"")!=0)||(strcmp(tmp3,"")!=0))
     strcpy(tmp,"1");
    break;
   case '!':
    eval(s,tmp2,i,akt,0);
    if((strcmp(tmp2,"")==0))
     strcpy(tmp,"1");
    break;
   case '\\':
    tmp[0]='\\';
    if(!out)
    {
     tmp[1]='\\';tmp[2]=0;
    }
    else
     tmp[1]=0;
    break;
  }
 }
 strcat(s1,tmp);
}

void parsuj(const char *s, char* s1)
{
 long i;
 for(i=0;i<strlen(s);eval(s,s1,&i,1,0));
}

int main(int argc,char* argv[])
{
 int dalej=1;
 char s[src_max_length],s1[src_max_length];
 if(argc<2) exit(0);
 FILE *src=fopen(argv[1],"r");
 if(argc>2)
 {
  deb=fopen(argv[2],"w");
  debug=1;
 }
 wczytaj(s,src);
 do
 {
  s1[0]=0;
  if(debug) {fputs(s,deb);fputc(10,deb);}
  parsuj(s,s1);
  strcpy(s,s1);
  if(!szukaj_O(s)) dalej=0;
 }
 while(dalej);
 fclose(src);
 if(debug) fclose(deb);
 return 0;
}
