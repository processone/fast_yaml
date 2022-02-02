/*
 * Copyright (C) 2002-2022 ProcessOne, SARL. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include <yaml.h>
#include <erl_nif.h>
#include <stdlib.h>
#include <assert.h>

#define OK 0
#define ERR_MEMORY_FAIL 1
#define PLAIN_AS_ATOM 1
#define SANE_SCALARS 2
#define MAPS 4

typedef struct events_t {
    yaml_event_t *event;
    struct events_t *next;
} events_t;

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static void unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
}

static ERL_NIF_TERM make_binary_size(ErlNifEnv* env,
                                     const unsigned char *str,
                                     size_t size)
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

static int make_num(ErlNifEnv* env, const unsigned char *value, size_t size, ERL_NIF_TERM *result)
{
    long int i;
    double d;
    *result = 0;

    if (size>0) {
        char *buf = enif_alloc(size + 1);
        if (buf) {
            memcpy(buf, value, size);
            buf[size] = '\0';
            char *check;
            i = strtol(buf, &check, 10);
            if (*check == '\0')
                *result = enif_make_long(env, i);
            else if (*check == '.') {
                d = strtod(buf, &check);
                if (*check == '\0')
                    *result = enif_make_double(env, d);
            }
            enif_free(buf);
            if (*result) {
                return 0;
            }
        }
    }

    return 1;
}

static int make_atom(ErlNifEnv* env, yaml_event_t *event)
{
    size_t length = event->data.scalar.length;
    yaml_char_t *value = event->data.scalar.value;
    ERL_NIF_TERM err;

    if (length > 255) {
        char *problem = "atom value must not exceed 255 octets in length";
        err = enif_make_tuple4(env,
                               enif_make_atom(env, "parser_error"),
                               make_binary(env, (unsigned char *) problem),
                               enif_make_uint(env, event->start_mark.line),
                               enif_make_uint(env, event->start_mark.column));
        return enif_raise_exception(env, err);
    } else {
        return enif_make_atom_len(env, (char *) value, length);
    }
}

static ERL_NIF_TERM make_scalar(ErlNifEnv* env, yaml_event_t *event, int flags, int is_map_value)
{
    int as_atom = PLAIN_AS_ATOM & flags;
    int sane_scalars = SANE_SCALARS & flags;

    yaml_scalar_style_t style = event->data.scalar.style;
    ERL_NIF_TERM rterm;

    if (sane_scalars) {
        if (is_map_value && style == YAML_PLAIN_SCALAR_STYLE) {
            if ((!make_num(env, event->data.scalar.value, event->data.scalar.length, &rterm))) {
                // rterm filled in make_num
            }
            else if (!strcmp((char *)event->data.scalar.value, "true")) {
                rterm = enif_make_atom(env, "true");
            }
            else if (!strcmp((char *)event->data.scalar.value, "false")) {
                rterm = enif_make_atom(env, "false");
            }
            else if (!event->data.scalar.length || !strcmp((char *)event->data.scalar.value, "null") || !strcmp((char *)event->data.scalar.value, "~")) {
                rterm = enif_make_atom(env, "undefined");
            } else {
                rterm = make_binary_size(env, event->data.scalar.value, event->data.scalar.length);
            }
        } else {
            rterm = make_binary_size(env, event->data.scalar.value, event->data.scalar.length);
        }
    } else if (as_atom && style == YAML_SINGLE_QUOTED_SCALAR_STYLE) {
        rterm = make_atom(env, event);
    } else if (style == YAML_DOUBLE_QUOTED_SCALAR_STYLE) {
        rterm = make_binary_size(env, event->data.scalar.value,
                                 event->data.scalar.length);
    } else if ((!make_num(env, event->data.scalar.value, event->data.scalar.length, &rterm))) {
        // rterm filled in make_num
    } else if (as_atom && style == YAML_PLAIN_SCALAR_STYLE && event->data.scalar.length) {
        rterm = make_atom(env, event);
    } else {
        rterm = make_binary_size(env, event->data.scalar.value,
                                 event->data.scalar.length);
    }

    return rterm;
}

static ERL_NIF_TERM make_alias(ErlNifEnv* env, yaml_event_t *event)
{
    return make_binary(env, event->data.alias.anchor);
}

static ERL_NIF_TERM zip(ErlNifEnv* env, ERL_NIF_TERM list)
{
    ERL_NIF_TERM key, val, tmp1, tmp2;

    if (enif_get_list_cell(env, list, &key, &tmp1)) {
        if (enif_get_list_cell(env, tmp1, &val, &tmp2)) {
            return enif_make_list_cell(env,
                                       enif_make_tuple2(env, key, val),
                                       zip(env, tmp2));
        } else {
            return enif_make_list_cell(env, key, enif_make_list(env, 0));
        }
    } else
        return list;
}

static ERL_NIF_TERM map(ErlNifEnv* env, ERL_NIF_TERM pairs)
{
    ERL_NIF_TERM ret;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;

    ret = enif_make_new_map(env);
    while(enif_get_list_cell(env, pairs, &val, &pairs)) {
        if (!enif_get_list_cell(env, pairs, &key, &pairs)) {
            assert(0 == 1 && "Unbalanced object pairs.");
        }
        if (!enif_make_map_put(env, ret, key, val, &ret)) {
            return 0;
        }
    }
    return ret;
}

static ERL_NIF_TERM make_error(ErlNifEnv* env, yaml_parser_t *parser)
{
    ERL_NIF_TERM err;

    switch (parser->error) {
    case YAML_MEMORY_ERROR:
        err = enif_make_atom(env, "memory_error");
        break;
    case YAML_PARSER_ERROR:
        err = enif_make_tuple4(env,
                               enif_make_atom(env, "parser_error"),
                               make_binary(env, (const unsigned char*) parser->problem),
                               enif_make_uint(env, parser->problem_mark.line),
                               enif_make_uint(env, parser->problem_mark.column));
        break;
    case YAML_SCANNER_ERROR:
        err = enif_make_tuple4(env,
                               enif_make_atom(env, "scanner_error"),
                               make_binary(env, (const unsigned char*) parser->problem),
                               enif_make_uint(env, parser->problem_mark.line),
                               enif_make_uint(env, parser->problem_mark.column));
        break;
    default:
        err = enif_make_atom(env, "unexpected_error");
        break;
    }

    return enif_make_tuple2(env, enif_make_atom(env, "error"), err);
}

static yaml_event_t *next(events_t **events)
{
    yaml_event_t *event = NULL;
    events_t *tmp;

    if (*events) {
        event = (*events)->event;
        tmp = *events;
        *events = (*events)->next;
        enif_free(tmp);
    }

    return event;
}

static void free_events(events_t **events)
{
    yaml_event_t *event = NULL;

    if (events) {
        while (*events) {
            event = next(events);
            if (event) {
                yaml_event_delete(event);
                enif_free(event);
            }
        }
    }
}

static ERL_NIF_TERM process_events(ErlNifEnv* env, events_t **events,
                                   yaml_parser_t *parser, int flags, int is_map)
{
    ERL_NIF_TERM els, el;
    yaml_event_t *event;
    els = enif_make_list(env, 0);
    int mapping_node = 0;

    if (events) {
        while (*events) {
            event = next(events);

            if (event) {
                switch (event->type) {
                case YAML_SEQUENCE_START_EVENT:
                    el = process_events(env, events, parser, flags, 0);
                    els = enif_make_list_cell(env, el, els);
                    mapping_node = 0;
                    break;
                case YAML_SEQUENCE_END_EVENT:
                    yaml_event_delete(event);
                    enif_free(event);
                    enif_make_reverse_list(env, els, &els);
                    return els;
                case YAML_MAPPING_START_EVENT:
                    el = process_events(env, events, parser, flags, 1);
                    els = enif_make_list_cell(env, el, els);
                    mapping_node = 0;
                    break;
                case YAML_MAPPING_END_EVENT:
                    yaml_event_delete(event);
                    enif_free(event);
                    if (MAPS & flags) {
                        return map(env, els);
                    }
                    enif_make_reverse_list(env, els, &els);
                    return zip(env, els);
                case YAML_SCALAR_EVENT:
                    el = make_scalar(env, event, flags, is_map ? (mapping_node++) % 2 : 1);
                    els = enif_make_list_cell(env, el, els);
                    break;
                case YAML_ALIAS_EVENT:
                    el = make_alias(env, event);
                    els = enif_make_list_cell(env, el, els);
                    break;
                default:
                    break;
                }
                yaml_event_delete(event);
                enif_free(event);
            } else {
                break;
            }
        }
    }

    return els;
}

static ERL_NIF_TERM parse(ErlNifEnv* env, yaml_parser_t *parser,
                          int flags, unsigned char *data, int size)
{
    int result = 0, done = 0;
    yaml_event_t *event = NULL;
    events_t *first_events = NULL;
    events_t *prev_events = NULL;
    events_t *events = NULL;
    ERL_NIF_TERM rterm;

    yaml_parser_set_input_string(parser, data, size);

    do {
        event = enif_alloc(sizeof(yaml_event_t));
        result = yaml_parser_parse(parser, event);
        if (result) {
            events = enif_alloc(sizeof(events_t));
            events->event = event;
            events->next = NULL;
            if (!first_events) {
                first_events = events;
            }
            if (prev_events) {
                prev_events->next = events;
            }
            prev_events = events;
            done = (event->type == YAML_STREAM_END_EVENT);
        } else {
            enif_free(event);
            done = 1;
        }
    } while (!done);

    if (result) {
        enif_make_reverse_list(env, process_events(env, &first_events, parser, flags, 0), &rterm);
        rterm = enif_make_tuple2(env, enif_make_atom(env, "ok"), rterm);
    } else {
        rterm = make_error(env, parser);
    }

    free_events(&first_events);
    return rterm;
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
            enif_get_uint(env, argv[1], &flags)) {
            yaml_parser_initialize(&parser);
            result = parse(env, &parser, flags, input.data, input.size);
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

ERL_NIF_INIT(fast_yaml, nif_funcs, load, reload, upgrade, unload)
