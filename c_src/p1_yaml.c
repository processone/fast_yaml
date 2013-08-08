#include <yaml.h>
#include <erl_nif.h>

#define OK 0
#define ERR_MEMORY_FAIL 1
#define PLAIN_AS_ATOM 1

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM make_mark(ErlNifEnv* env, yaml_mark_t *mark)
{
    return enif_make_tuple4(env,
			    enif_make_atom(env, "mark"),
			    enif_make_uint(env, (unsigned int) mark->index),
			    enif_make_uint(env, (unsigned int) mark->line),
			    enif_make_uint(env, (unsigned int) mark->column));
}

static ERL_NIF_TERM make_bool(ErlNifEnv* env, int flag)
{
    if (flag)
	return enif_make_atom(env, "true");
    else
	return enif_make_atom(env, "false");
}

static ERL_NIF_TERM make_binary_size(ErlNifEnv* env, const unsigned char *str, size_t size)
{
    ErlNifBinary b;
    enif_alloc_binary(size, &b);

    if (str) memcpy(b.data, str, size);

    return enif_make_binary(env, &b);
}
	
static ERL_NIF_TERM make_binary(ErlNifEnv* env, const unsigned char *str)
{
    size_t size = 0;

    if (str) size = strlen((char *) str);
    return make_binary_size(env, str, size);
}

static ERL_NIF_TERM make_document_start(ErlNifEnv* env, yaml_event_t *event)
{
    /* TODO: make tag directives */

    ERL_NIF_TERM version_directive;
    if (event->data.document_start.version_directive) {
	int major = event->data.document_start.version_directive->major;
	int minor = event->data.document_start.version_directive->minor;
	version_directive = enif_make_tuple2(env,
					     enif_make_uint(env, major),
					     enif_make_uint(env, minor));
    } else {
	version_directive = enif_make_atom(env, "undefined");
    }

    return enif_make_tuple5(env,
			    enif_make_atom(env, "document_start"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark),
			    version_directive,
			    make_bool(env, event->data.document_start.implicit));
}

static ERL_NIF_TERM make_document_end(ErlNifEnv* env, yaml_event_t *event)
{
    return enif_make_tuple4(env,
			    enif_make_atom(env, "document_end"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark),
			    make_bool(env, event->data.document_end.implicit));
}

static ERL_NIF_TERM make_scalar(ErlNifEnv* env, yaml_event_t *event)
{
    ERL_NIF_TERM style;
    switch (event->data.scalar.style) {
    case YAML_ANY_SCALAR_STYLE:
	style = enif_make_atom(env, "any");
	break;
    case YAML_PLAIN_SCALAR_STYLE:
	style = enif_make_atom(env, "plain");
	break;
    case YAML_SINGLE_QUOTED_SCALAR_STYLE:
	style = enif_make_atom(env, "single_quoted");
	break;
    case YAML_DOUBLE_QUOTED_SCALAR_STYLE:
	style = enif_make_atom(env, "double_quoted");
	break;
    case YAML_LITERAL_SCALAR_STYLE:
	style = enif_make_atom(env, "literal");
	break;
    case YAML_FOLDED_SCALAR_STYLE:
	style = enif_make_atom(env, "folded");
	break;
    default:
	style = enif_make_atom(env, "undefined");
	break;
    }
    
    return enif_make_tuple9(env,
			    enif_make_atom(env, "scalar"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark),
			    style,
			    make_binary(env, event->data.scalar.anchor),
			    make_binary(env, event->data.scalar.tag),
			    make_binary_size(env, event->data.scalar.value,
					     event->data.scalar.length),
			    make_bool(env, event->data.scalar.plain_implicit),
			    make_bool(env, event->data.scalar.quoted_implicit));
}

static ERL_NIF_TERM make_alias(ErlNifEnv* env, yaml_event_t *event)
{
    return enif_make_tuple4(env,
			    enif_make_atom(env, "alias"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark),
			    make_binary(env, event->data.scalar.anchor));
}

static ERL_NIF_TERM make_stream_start(ErlNifEnv* env, yaml_event_t *event)
{
    ERL_NIF_TERM encoding;

    switch (event->data.stream_start.encoding) {
    case YAML_ANY_ENCODING:
	encoding = enif_make_atom(env, "any");
	break;
    case YAML_UTF8_ENCODING:
	encoding = enif_make_atom(env, "utf8");
	break;
    case YAML_UTF16LE_ENCODING:
	encoding = enif_make_atom(env, "utf16-le");
	break;
    case YAML_UTF16BE_ENCODING:
	encoding = enif_make_atom(env, "utf16-be");
	break;
    default:
	encoding = enif_make_atom(env, "undefined");
	break;
    }
    
    return enif_make_tuple4(env,
			    enif_make_atom(env, "stream_start"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark),
			    encoding);
}

static ERL_NIF_TERM make_stream_end(ErlNifEnv* env, yaml_event_t *event)
{
    return enif_make_tuple3(env,
			    enif_make_atom(env, "stream_end"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark));
}

static ERL_NIF_TERM make_mapping_end(ErlNifEnv* env, yaml_event_t *event)
{
    return enif_make_tuple3(env,
			    enif_make_atom(env, "mapping_end"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark));
}

static ERL_NIF_TERM make_sequence_end(ErlNifEnv* env, yaml_event_t *event)
{
    return enif_make_tuple3(env,
			    enif_make_atom(env, "sequence_end"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark));
}

static ERL_NIF_TERM make_sequence_start(ErlNifEnv* env, yaml_event_t *event)
{
    ERL_NIF_TERM style;
    switch (event->data.sequence_start.style) {
    case YAML_ANY_SEQUENCE_STYLE:
	style = enif_make_atom(env, "any");
	break;
    case YAML_BLOCK_SEQUENCE_STYLE:
	style = enif_make_atom(env, "block");
	break;
    case YAML_FLOW_SEQUENCE_STYLE:
	style = enif_make_atom(env, "flow");
	break;
    default:
	style = enif_make_atom(env, "undefined");
	break;
    }

    return enif_make_tuple7(env,
			    enif_make_atom(env, "sequence_start"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark),
			    style,
			    make_binary(env, event->data.sequence_start.anchor),
			    make_binary(env, event->data.sequence_start.tag),
			    make_bool(env, event->data.sequence_start.implicit));
}

static ERL_NIF_TERM make_mapping_start(ErlNifEnv* env, yaml_event_t *event)
{
    ERL_NIF_TERM style;
    switch (event->data.mapping_start.style) {
    case YAML_ANY_MAPPING_STYLE:
	style = enif_make_atom(env, "any");
	break;
    case YAML_BLOCK_MAPPING_STYLE:
	style = enif_make_atom(env, "block");
	break;
    case YAML_FLOW_MAPPING_STYLE:
	style = enif_make_atom(env, "flow");
	break;
    default:
	style = enif_make_atom(env, "undefined");
	break;
    }

    return enif_make_tuple7(env,
			    enif_make_atom(env, "mapping_start"),
			    make_mark(env, &event->start_mark),
			    make_mark(env, &event->end_mark),
			    style,
			    make_binary(env, event->data.mapping_start.anchor),
			    make_binary(env, event->data.mapping_start.tag),
			    make_bool(env, event->data.mapping_start.implicit));
}

static ERL_NIF_TERM make_error(ErlNifEnv* env, yaml_parser_t *parser)
{
    return enif_make_tuple3(env,
			    enif_make_atom(env, "stream_error"),
			    make_mark(env, &parser->problem_mark),
			    make_binary(env, (unsigned char*) parser->problem));
}

static ERL_NIF_TERM parse(ErlNifEnv* env, yaml_parser_t *parser,
			  unsigned char *data, int size)
{
    yaml_event_t event;
    ERL_NIF_TERM term, tail;
    int done;

    tail = enif_make_list(env, 0);

    yaml_parser_set_input_string(parser, data, size);

    do {
	if (yaml_parser_parse(parser, &event)) {
	    switch (event.type) {
	    case YAML_STREAM_START_EVENT:
		term = make_stream_start(env, &event);
		break;
	    case YAML_STREAM_END_EVENT:
		term = make_stream_end(env, &event);
		break;
	    case YAML_DOCUMENT_START_EVENT:
		term = make_document_start(env, &event);
		break;
	    case YAML_DOCUMENT_END_EVENT:
		term = make_document_end(env, &event);
		break;
	    case YAML_ALIAS_EVENT:
		term = make_alias(env, &event);
		break;
	    case YAML_SCALAR_EVENT:
		term = make_scalar(env, &event);
		break;
	    case YAML_SEQUENCE_START_EVENT:
		term = make_sequence_start(env, &event);
		break;
	    case YAML_SEQUENCE_END_EVENT:
		term = make_sequence_end(env, &event);
		break;
	    case YAML_MAPPING_START_EVENT:
		term = make_mapping_start(env, &event);
		break;
	    case YAML_MAPPING_END_EVENT:
		term = make_mapping_end(env, &event);
		break;
	    default:
		term = enif_make_atom(env, "unknown_event");
		break;
	    };

	    tail = enif_make_list_cell(env, term, tail);
	    
	    done = (event.type == YAML_STREAM_END_EVENT);

	    yaml_event_delete(&event);
	} else {
	    term = make_error(env, parser);
	    tail = enif_make_list_cell(env, term, tail);
	    done = 1;
	}
    } while (!done);

    return tail;
}

static ERL_NIF_TERM decode(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    ERL_NIF_TERM result;
    unsigned int flags;
    yaml_parser_t parser;

    if (argc == 2) {
	if (enif_inspect_iolist_as_binary(env, argv[0], &input) &&
	    enif_get_uint(env, argv[1], &flags))
	    {
		yaml_parser_initialize(&parser);
		result = parse(env, &parser, input.data, input.size);
		yaml_parser_delete(&parser);
		return result;
	    }
    }

    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] =
    {
	{"nif_decode", 2, decode}
    };

ERL_NIF_INIT(p1_yaml, nif_funcs, load, NULL, NULL, NULL)
