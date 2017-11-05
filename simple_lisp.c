//A simple parser and runtime for ninimal LISP
//
#include<assert.h>
#include<ctype.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define MAX_NAME_LEN 32
#define ENV_INIT_SIZE 8
#define LIST_MAX_SIZE 32
#define SRC_INIT_LEN 1024

#define DISPLAY_LIST_MAX_ITEM 3

enum Element{
	TYPE_INT,TYPE_BOOL,TYPE_NAME,TYPE_LIST,TYPE_BUILTIN,TYPE_LAMBDA,TYPE_NULL

};

struct ElementList;
struct Env;

struct Lambda{
	char *args[MAX_NAME_LEN];
	int arg_count;
	struct Env *parent;
	struct ElementList *body;
};

typedef struct Element *(*Builtin)(struct Element **,int,struct Env *);

struct Element{
	enum ElementType type;
	union{
		int int_value;
		int bool_value;
		char *name_value;
		struct ElementList *list_value;
		Builtin builtin_value;
		struct Lambda *lambda_value;
	}value;
};

struct ElementList {
  struct Element **elements;
  int length;
};

struct EnvPair {
  char *name;
  struct Element *value;
};

struct Env {
  struct EnvPair **values;
  int size;
  int length;
  struct Env *parent;
};

struct Env *creat_env(struct Env *parent)
{
	struct Env *env = malloc(sizeof(struct Env));
  env->parent = parent;
  env->values = malloc(sizeof(struct EnvPair *) * ENV_INIT_SIZE);
  env->size = ENV_INIT_SIZE;
  env->length = 0;
  return env;
}

struct Element *resolve(char *name, struct Env *env) {
  for (int i = 0; i < env->length; i++) {
    if (strcmp(name, env->values[i]->name) == 0) {
      return env->values[i]->value;
    }
  }
  if (env->parent == NULL) {
    fprintf(stderr, "there is no var called %s\n", name);
    exit(1);
  } else {
    return resolve(name, env->parent);
  }
}


void register(struct Env *env,char *name,struct Element *value)
{
	for(int i=0;i<env->length;i++)
	{
		 if (strcmp(name, env->values[i]->name) == 0) {
      env->values[i]->value = value;
      return;}

	}
	if (env->length == env->size) {
    env->values =
        realloc(env->values, sizeof(struct EnvPair *) * env->size * 2);
    env->size *= 2;
  }
  env->values[env->length] = malloc(sizeof(struct EnvPair));
  env->values[env->length]->name = name;
  env->values[env->length]->value = value;
  env->length++;
}


struct Element *apply(struct Element *ele,struct Element **args,int arg_count,struct Env *parent);

struct Element *eval_lambda(struct Element *lambda, struct Env *parent) {
  assert(lambda->value.list_value->length >= 2);
  struct Element *ele = malloc(sizeof(struct Element));
  ele->type = TYPE_LAMBDA;
  ele->value.lambda_value = malloc(sizeof(struct Lambda));
  ele->value.lambda_value->parent = parent;
  struct Element *arg_list = lambda->value.list_value->elements[1];
  assert(arg_list->type == TYPE_LIST);
  ele->value.lambda_value->arg_count = arg_list->value.list_value->length;
  for (int i = 0; i < ele->value.lambda_value->arg_count; i++) {
    assert(arg_list->value.list_value->elements[i]->type == TYPE_NAME);
    ele->value.lambda_value->args[i] =
        arg_list->value.list_value->elements[i]->value.name_value;
  }
  ele->value.lambda_value->body = malloc(sizeof(struct ElementList));
  int body_length = lambda->value.list_value->length - 2;
  ele->value.lambda_value->body->length = body_length;
  ele->value.lambda_value->body->elements =
      malloc(sizeof(struct Element *) * body_length);
  for (int i = 0; i < body_length; i++) {
    ele->value.lambda_value->body->elements[i] =
        lambda->value.list_value->elements[i + 2];
  }
  return ele;
}

struct Element *eval(struct Element *ele,struct Env *env);

struct Element *eval_cond(struct Element *cond,struct Env *parent)
{
	assert(cond->value.list_value->length>=2);
	for(int i=1;i<cond->value.list_value->length;i++)
	{
		struct Element *branch=cond->value.list_value->element[i];
		assert(branch->type==TYPE_LIST);
		assert(branch->value.list_value->length==2);
		struct Element *branch_cond=branch->value.list_value->elements[0];
		if(branch_cond->type==TYPE_NAME&&strcmp(branch_cond->value.name_value,"else")==0)
		{
			return eval(branch->value.list_value->elements[1],parent);
		}
		struct Element *condition=eval(branch_cond,parent);
		assert(condition->type==TYPE_BOOL);
		 if (condition->value.bool_value) {
      return eval(branch->value.list_value->elements[1], parent);
    }
	}

	struct Element *ele=malloc(sizeof(struct Element));
	ele->type=TYPE_NULL;
	return ele;
}


struct Element *eval_define(struct Element *def, struct Env *parent) {
  assert(def->value.list_value->length == 3);
  struct Element *name = def->value.list_value->elements[1];
  // dynamic name is not allowed
     assert(name->type == TYPE_NAME);
       register_(parent, name->value.name_value,
                   eval(def->value.list_value->elements[2], parent));
  
                     struct Element *ele = malloc(sizeof(struct Element));
                       ele->type = TYPE_NULL;
                         return ele;
                         }


struct Element *eval(struct Element *ele,struct Env *env)
{
	switch(ele->type)
	{
		case TYPE_INT:
		case TYPE_BOOL:
		case TYPE_BUILTIN:
		case TYPE_LAMBDA;
		case TYPE_NULL:
		 return ele;
		case TYPE_NAME:
		return resolve(ele->value.name_value,env);
		case TYPE_LIST:
		{



		struct Element *callable = ele->value.list_value->elements[0];
    if (callable->type == TYPE_NAME) {
      if (strcmp(callable->value.name_value, "lambda") == 0) {
        return eval_lambda(ele, env);
      } else if (strcmp(callable->value.name_value, "cond") == 0) {
        return eval_cond(ele, env);
      } else if (strcmp(callable->value.name_value, "define") == 0) {
        return eval_define(ele, env);
      } else {
        callable = resolve(callable->value.name_value, env);
      }
}
	
     if(callable->type==TYPE_LIST){
	{
		callable=eval(callable,env);
	}
	int arg_count=ele->value.list_value->length-1;
	struct Element **args=malloc(sizeof(struct Element *)*arg_count);
	for(int i=0;i<arg_count;i++)
	{
		arg[i]=eval(ele->value.list_value->elements[i+1],env);
	}
	return apply(callable,args,arg_count,env);
        }
}
}


struct Element *apply(struct Element *ele,struct Element **args,int arg_count,struct Env *parent)
{
	if(ele->type!=TYPE_LAMBDA &&ele->type!=TYPE_BUILTIN)
	{
		fprintf(stderr,"apply an un-callable element\n");
		exit(1);
	}
	if(ele->type==TYPE_BUILTIN)
	{
		return ele->value.builtin_value(args,arg_count,parent);
	}
	else
	{
		assert(ele->value.lambda_value->arg_count==arg_count);
		struct Env *env=create_new(ele->value.lambda_value->parent);
		for(int i=0;i<ele->value.lambda_value->arg_count;i++)
		{
			register_(env,ele->value.lambda_value->args[i],args[i]);
		}
		struct Element *result=malooc(sizeof(struct Element));
		result->type=TYPE_NULL;
		for(int i=0;i<ele->value.lambda_value->body->length;i++)
		{
			result=eval(ele->value.lambda_value->body->element[i],env);
		}
		return result;
	}
}
 
