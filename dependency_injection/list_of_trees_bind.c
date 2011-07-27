/*
** Lua binding: list_of_trees
** Generated automatically by tolua++-1.0.92 on Wed Jul 20 13:23:24 2011.
*/

#ifndef __cplusplus
#include "stdlib.h"
#endif
#include "string.h"

#include "tolua++.h"

/* Exported function */
TOLUA_API int  tolua_list_of_trees_open (lua_State* tolua_S);


/* function to release collected object via destructor */
#ifdef __cplusplus

static int tolua_collect_Leaf (lua_State* tolua_S)
{
 Leaf* self = (Leaf*) tolua_tousertype(tolua_S,1,0);
	delete self;
	return 0;
}

static int tolua_collect_Empty (lua_State* tolua_S)
{
 Empty* self = (Empty*) tolua_tousertype(tolua_S,1,0);
	delete self;
	return 0;
}

static int tolua_collect_Nonempty (lua_State* tolua_S)
{
 Nonempty* self = (Nonempty*) tolua_tousertype(tolua_S,1,0);
	delete self;
	return 0;
}

static int tolua_collect_Branch (lua_State* tolua_S)
{
 Branch* self = (Branch*) tolua_tousertype(tolua_S,1,0);
	delete self;
	return 0;
}
#endif


/* function to register type */
static void tolua_reg_types (lua_State* tolua_S)
{
 tolua_usertype(tolua_S,"Leaf");
 tolua_usertype(tolua_S,"Nonempty");
 tolua_usertype(tolua_S,"ListOfTrees");
 tolua_usertype(tolua_S,"Empty");
 tolua_usertype(tolua_S,"Tree");
 tolua_usertype(tolua_S,"Branch");
}

/* method: new of class  Empty */
#ifndef TOLUA_DISABLE_tolua_list_of_trees_Empty_new00
static int tolua_list_of_trees_Empty_new00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
     !tolua_isusertable(tolua_S,1,"Empty",0,&tolua_err) ||
     !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
  goto tolua_lerror;
 else
#endif
 {
  {
   Empty* tolua_ret = (Empty*)  new Empty();
   tolua_pushusertype(tolua_S,(void*)tolua_ret,"Empty");
  }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new'.",&tolua_err);
 return 0;
#endif
}
#endif //#ifndef TOLUA_DISABLE

/* method: new_local of class  Empty */
#ifndef TOLUA_DISABLE_tolua_list_of_trees_Empty_new00_local
static int tolua_list_of_trees_Empty_new00_local(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
     !tolua_isusertable(tolua_S,1,"Empty",0,&tolua_err) ||
     !tolua_isnoobj(tolua_S,2,&tolua_err)
 )
  goto tolua_lerror;
 else
#endif
 {
  {
   Empty* tolua_ret = (Empty*)  new Empty();
   tolua_pushusertype_and_takeownership(tolua_S,(void *)tolua_ret,"Empty");
  }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new'.",&tolua_err);
 return 0;
#endif
}
#endif //#ifndef TOLUA_DISABLE

/* method: new of class  Nonempty */
#ifndef TOLUA_DISABLE_tolua_list_of_trees_Nonempty_new00
static int tolua_list_of_trees_Nonempty_new00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
     !tolua_isusertable(tolua_S,1,"Nonempty",0,&tolua_err) ||
     !tolua_isusertype(tolua_S,2,"Tree",0,&tolua_err) ||
     !tolua_isusertype(tolua_S,3,"ListOfTrees",0,&tolua_err) ||
     !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
  goto tolua_lerror;
 else
#endif
 {
  Tree* first = ((Tree*)  tolua_tousertype(tolua_S,2,0));
  ListOfTrees* rest = ((ListOfTrees*)  tolua_tousertype(tolua_S,3,0));
  {
   Nonempty* tolua_ret = (Nonempty*)  new Nonempty(first,rest);
   tolua_pushusertype(tolua_S,(void*)tolua_ret,"Nonempty");
  }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new'.",&tolua_err);
 return 0;
#endif
}
#endif //#ifndef TOLUA_DISABLE

/* method: new_local of class  Nonempty */
#ifndef TOLUA_DISABLE_tolua_list_of_trees_Nonempty_new00_local
static int tolua_list_of_trees_Nonempty_new00_local(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
     !tolua_isusertable(tolua_S,1,"Nonempty",0,&tolua_err) ||
     !tolua_isusertype(tolua_S,2,"Tree",0,&tolua_err) ||
     !tolua_isusertype(tolua_S,3,"ListOfTrees",0,&tolua_err) ||
     !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
  goto tolua_lerror;
 else
#endif
 {
  Tree* first = ((Tree*)  tolua_tousertype(tolua_S,2,0));
  ListOfTrees* rest = ((ListOfTrees*)  tolua_tousertype(tolua_S,3,0));
  {
   Nonempty* tolua_ret = (Nonempty*)  new Nonempty(first,rest);
   tolua_pushusertype_and_takeownership(tolua_S,(void *)tolua_ret,"Nonempty");
  }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new'.",&tolua_err);
 return 0;
#endif
}
#endif //#ifndef TOLUA_DISABLE

/* method: new of class  Leaf */
#ifndef TOLUA_DISABLE_tolua_list_of_trees_Leaf_new00
static int tolua_list_of_trees_Leaf_new00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
     !tolua_isusertable(tolua_S,1,"Leaf",0,&tolua_err) ||
     !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
     !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
  goto tolua_lerror;
 else
#endif
 {
  int value = ((int)  tolua_tonumber(tolua_S,2,0));
  {
   Leaf* tolua_ret = (Leaf*)  new Leaf(value);
   tolua_pushusertype(tolua_S,(void*)tolua_ret,"Leaf");
  }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new'.",&tolua_err);
 return 0;
#endif
}
#endif //#ifndef TOLUA_DISABLE

/* method: new_local of class  Leaf */
#ifndef TOLUA_DISABLE_tolua_list_of_trees_Leaf_new00_local
static int tolua_list_of_trees_Leaf_new00_local(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
     !tolua_isusertable(tolua_S,1,"Leaf",0,&tolua_err) ||
     !tolua_isnumber(tolua_S,2,0,&tolua_err) ||
     !tolua_isnoobj(tolua_S,3,&tolua_err)
 )
  goto tolua_lerror;
 else
#endif
 {
  int value = ((int)  tolua_tonumber(tolua_S,2,0));
  {
   Leaf* tolua_ret = (Leaf*)  new Leaf(value);
   tolua_pushusertype_and_takeownership(tolua_S,(void *)tolua_ret,"Leaf");
  }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new'.",&tolua_err);
 return 0;
#endif
}
#endif //#ifndef TOLUA_DISABLE

/* method: new of class  Branch */
#ifndef TOLUA_DISABLE_tolua_list_of_trees_Branch_new00
static int tolua_list_of_trees_Branch_new00(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
     !tolua_isusertable(tolua_S,1,"Branch",0,&tolua_err) ||
     !tolua_isusertype(tolua_S,2,"Tree",0,&tolua_err) ||
     !tolua_isusertype(tolua_S,3,"Tree",0,&tolua_err) ||
     !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
  goto tolua_lerror;
 else
#endif
 {
  Tree* left = ((Tree*)  tolua_tousertype(tolua_S,2,0));
  Tree* right = ((Tree*)  tolua_tousertype(tolua_S,3,0));
  {
   Branch* tolua_ret = (Branch*)  new Branch(left,right);
   tolua_pushusertype(tolua_S,(void*)tolua_ret,"Branch");
  }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new'.",&tolua_err);
 return 0;
#endif
}
#endif //#ifndef TOLUA_DISABLE

/* method: new_local of class  Branch */
#ifndef TOLUA_DISABLE_tolua_list_of_trees_Branch_new00_local
static int tolua_list_of_trees_Branch_new00_local(lua_State* tolua_S)
{
#ifndef TOLUA_RELEASE
 tolua_Error tolua_err;
 if (
     !tolua_isusertable(tolua_S,1,"Branch",0,&tolua_err) ||
     !tolua_isusertype(tolua_S,2,"Tree",0,&tolua_err) ||
     !tolua_isusertype(tolua_S,3,"Tree",0,&tolua_err) ||
     !tolua_isnoobj(tolua_S,4,&tolua_err)
 )
  goto tolua_lerror;
 else
#endif
 {
  Tree* left = ((Tree*)  tolua_tousertype(tolua_S,2,0));
  Tree* right = ((Tree*)  tolua_tousertype(tolua_S,3,0));
  {
   Branch* tolua_ret = (Branch*)  new Branch(left,right);
   tolua_pushusertype_and_takeownership(tolua_S,(void *)tolua_ret,"Branch");
  }
 }
 return 1;
#ifndef TOLUA_RELEASE
 tolua_lerror:
 tolua_error(tolua_S,"#ferror in function 'new'.",&tolua_err);
 return 0;
#endif
}
#endif //#ifndef TOLUA_DISABLE

/* Open function */
TOLUA_API int tolua_list_of_trees_open (lua_State* tolua_S)
{
 tolua_open(tolua_S);
 tolua_reg_types(tolua_S);
 tolua_module(tolua_S,NULL,0);
 tolua_beginmodule(tolua_S,NULL);
  tolua_cclass(tolua_S,"ListOfTrees","ListOfTrees","",NULL);
  tolua_beginmodule(tolua_S,"ListOfTrees");
  tolua_endmodule(tolua_S);
  tolua_cclass(tolua_S,"Tree","Tree","",NULL);
  tolua_beginmodule(tolua_S,"Tree");
  tolua_endmodule(tolua_S);
  #ifdef __cplusplus
  tolua_cclass(tolua_S,"Empty","Empty","ListOfTrees",tolua_collect_Empty);
  #else
  tolua_cclass(tolua_S,"Empty","Empty","ListOfTrees",NULL);
  #endif
  tolua_beginmodule(tolua_S,"Empty");
   tolua_function(tolua_S,"new",tolua_list_of_trees_Empty_new00);
   tolua_function(tolua_S,"new_local",tolua_list_of_trees_Empty_new00_local);
   tolua_function(tolua_S,".call",tolua_list_of_trees_Empty_new00_local);
  tolua_endmodule(tolua_S);

  { /* begin embedded lua code */
   int top = lua_gettop(tolua_S);
   static unsigned char B[] = {
    10,102,117,110, 99,116,105,111,110, 32, 76, 69,109,112,116,
    121, 40, 97,114,103,115,116, 97, 98,108,101, 41, 10,114,101,
    116,117,114,110, 32, 69,109,112,116,121, 58,110,101,119, 40,
     41, 10,101,110,100,32
   };
   tolua_dobuffer(tolua_S,(char*)B,sizeof(B),"tolua: embedded Lua code 1");
   lua_settop(tolua_S, top);
  } /* end of embedded lua code */

  #ifdef __cplusplus
  tolua_cclass(tolua_S,"Nonempty","Nonempty","ListOfTrees",tolua_collect_Nonempty);
  #else
  tolua_cclass(tolua_S,"Nonempty","Nonempty","ListOfTrees",NULL);
  #endif
  tolua_beginmodule(tolua_S,"Nonempty");
   tolua_function(tolua_S,"new",tolua_list_of_trees_Nonempty_new00);
   tolua_function(tolua_S,"new_local",tolua_list_of_trees_Nonempty_new00_local);
   tolua_function(tolua_S,".call",tolua_list_of_trees_Nonempty_new00_local);
  tolua_endmodule(tolua_S);

  { /* begin embedded lua code */
   int top = lua_gettop(tolua_S);
   static unsigned char B[] = {
    10,102,117,110, 99,116,105,111,110, 32, 76, 78,111,110,101,
    109,112,116,121, 40, 97,114,103,115,116, 97, 98,108,101, 41,
     10,114,101,116,117,114,110, 32, 78,111,110,101,109,112,116,
    121, 40, 97,114,103,115,116, 97, 98,108,101, 46,102,105,114,
    115,116, 44, 32, 97,114,103,115,116, 97, 98,108,101, 46,114,
    101,115,116, 41, 10,101,110,100,32
   };
   tolua_dobuffer(tolua_S,(char*)B,sizeof(B),"tolua: embedded Lua code 2");
   lua_settop(tolua_S, top);
  } /* end of embedded lua code */

  #ifdef __cplusplus
  tolua_cclass(tolua_S,"Leaf","Leaf","Tree",tolua_collect_Leaf);
  #else
  tolua_cclass(tolua_S,"Leaf","Leaf","Tree",NULL);
  #endif
  tolua_beginmodule(tolua_S,"Leaf");
   tolua_function(tolua_S,"new",tolua_list_of_trees_Leaf_new00);
   tolua_function(tolua_S,"new_local",tolua_list_of_trees_Leaf_new00_local);
   tolua_function(tolua_S,".call",tolua_list_of_trees_Leaf_new00_local);
  tolua_endmodule(tolua_S);

  { /* begin embedded lua code */
   int top = lua_gettop(tolua_S);
   static unsigned char B[] = {
    10,102,117,110, 99,116,105,111,110, 32, 76, 76,101, 97,102,
     40, 97,114,103,115,116, 97, 98,108,101, 41, 10,114,101,116,
    117,114,110, 32, 76,101, 97,102, 58,110,101,119, 40, 97,114,
    103,115,116, 97, 98,108,101, 46,118, 97,108,117,101, 41, 10,
    101,110,100,32
   };
   tolua_dobuffer(tolua_S,(char*)B,sizeof(B),"tolua: embedded Lua code 3");
   lua_settop(tolua_S, top);
  } /* end of embedded lua code */

  #ifdef __cplusplus
  tolua_cclass(tolua_S,"Branch","Branch","Tree",tolua_collect_Branch);
  #else
  tolua_cclass(tolua_S,"Branch","Branch","Tree",NULL);
  #endif
  tolua_beginmodule(tolua_S,"Branch");
   tolua_function(tolua_S,"new",tolua_list_of_trees_Branch_new00);
   tolua_function(tolua_S,"new_local",tolua_list_of_trees_Branch_new00_local);
   tolua_function(tolua_S,".call",tolua_list_of_trees_Branch_new00_local);
  tolua_endmodule(tolua_S);

  { /* begin embedded lua code */
   int top = lua_gettop(tolua_S);
   static unsigned char B[] = {
    10,102,117,110, 99,116,105,111,110, 32, 76, 66,114, 97,110,
     99,104, 40, 97,114,103,115,116, 97, 98,108,101, 41, 10,114,
    101,116,117,114,110, 32, 66,114, 97,110, 99,104, 58,110,101,
    119, 40, 97,114,103,115,116, 97, 98,108,101, 46,101,102,116,
     44, 32, 97,114,103,115,116, 97, 98,108,101, 46,114,105,103,
    104,116, 41, 10,101,110,100,32
   };
   tolua_dobuffer(tolua_S,(char*)B,sizeof(B),"tolua: embedded Lua code 4");
   lua_settop(tolua_S, top);
  } /* end of embedded lua code */

 tolua_endmodule(tolua_S);
 return 1;
}


#if defined(LUA_VERSION_NUM) && LUA_VERSION_NUM >= 501
 TOLUA_API int luaopen_list_of_trees (lua_State* tolua_S) {
 return tolua_list_of_trees_open(tolua_S);
};
#endif

