#!/usr/bin/env python3
"""
    A BLC parser/interpreter in python

    :created: 2019-12-14 12:35:36 -0800
    :copyright: (c) 2019, Stephen A. Balaban
    :license: MIT
"""
import sys
import os
import argparse
import tqdm
import numpy as np
import anytree
import numpy
from matplotlib import pyplot as plt
import multiprocessing
import ctypes


# Non-terminals
NT_LAMBDA = '00'
NT_APPLY = '01'
NT_COMMENT_BEGIN = '#'
NT_NEWLINE = '\n'
NT_LAMBDA_LAM = '\\'

# Compiler output
EVALUATION_HEADER = '#!/usr/bin/env blc'

# Language extensions
BLC = 'blc'  # .blc files
LAM = 'lam'  # .lam files
DEFAULT_LANG = BLC

# Terminals
LAMBDA = '?'
APPLY = 'A'
COMMENT_BEGIN = 'COMMENT_BEGIN'
NEWLINE = 'NEWLINE'
NOP = None

# Grammar
# 1^{n+1}0  = int
# 00M       = (LAMBDA, M)
# 01MN      = [M, N]

# Shell settings
DEFAULT_PS1 = '> '


class CycleException(Exception):
    pass


def IS_VAR(two):
    return two == '10' or two == '11'


def int_to_debruijn(integer):
    return '1'*(integer + 1) + '0'


def lex(source_string):
    i = 0
    while True:
        try:
            two = source_string[i] + source_string[i + 1]
        except IndexError:
            break
        if two == NT_LAMBDA:
            i = i + 2
            yield LAMBDA
        elif two == NT_APPLY:
            i = i + 2
            yield APPLY
        elif IS_VAR(two):
            n = source_string[i + 1]
            msg = "Must be either 0, 1. Got = '" + n + "'"
            assert n == '0' or n == '1', msg
            dbindex = 0
            while n != '0':
                dbindex += 1
                n = source_string[i + 1 + dbindex]
            i = i + 2 + dbindex
            yield dbindex
        elif two[0] == NT_COMMENT_BEGIN:
            i = i + 1
            yield COMMENT_BEGIN
        elif two[0] == NT_NEWLINE:
            i = i + 1
            yield NEWLINE
        else:
            i = i + 1
            yield two[0]
            # assert False, "Not a valid character. {}".format(two)


def body(expr):
    """Gets the body of a parsed lambda (represented as 2-tuples)"""
    assert type(expr) == tuple
    return expr[1]


def end_of_first_lambda(a):
    for index in range(len(a)):
        c = a[index]
        cn = a[index + 1]
        if type(c) == int and (cn == LAMBDA or cn == APPLY):
            return index + 1


def find_argument(a):
    """
    start of first lambda or free variable reading right (this is the b portion
    of an (a b) application).
    """
    if len(a) == 3:
        x, y, z = a
        if x == APPLY and type(y) == int and type(z) == int:
            return 2
    for index in reversed(range(len(a))):
        c = a[index]
        cp = a[index - 1]
        if c == LAMBDA and type(cp) == int:
            return index
        if c == APPLY and type(cp) == int:
            return index
    assert False, "Couldn't find a lambda or variable, malformed code."


def linecount(toklist):
    return sum(1 for tok in toklist if tok == NEWLINE)


def parse(tokens):
    """
    Every time you generate a statement, backtrack to see if you have completed
    an application.
    """
    revlist = list(reversed(tokens))
    stack = []
    line = linecount(revlist)
    col = 1
    for tok in revlist:
        col += 1
        try:
            if type(tok) == int:
                r = parse_var(tok, stack)
            elif tok == APPLY:
                r = parse_apply(tok, stack)
            elif tok == LAMBDA:
                r = parse_lambda(tok, stack)
            elif tok == NEWLINE:
                col = 1
                line -= 1
                r = NOP
            else:
                # Some other token that's unknown, probably a comment.
                r = NOP
            if r is not NOP:
                stack.insert(0, r)
        except IndexError:
            raise SyntaxError("Invalid syntax at: Line {}, Column {}"
                              .format(line, col))
    return stack.pop(0)


def parse_lambda(tok, context):
    body = context.pop(0)
    return (LAMBDA, body)


def parse_apply(tok, context):
    lhs = context.pop(0)
    rhs = context.pop(0)
    return [lhs, rhs]


def parse_var(tok, context):
    return tok


def slurp(filepath):
    if filepath == '-':
        return sys.stdin.read()
    with open(filepath) as f:
        return f.read()


def spit(filepath, body, executable=False):
    if filepath == '-':
        sys.stdout.write(body)
        sys.stdout.flush()
    else:
        with open(filepath, 'w+') as f:
            f.write(body)
        if executable:
            os.chmod(filepath, 0o755)


def read(verbose=False):
    raw_text = ''.join(sys.stdin)
    if verbose:
        print("raw text: " + raw_text)
    return parse_blc_raw(raw_text)


def parse_blc_raw(raw_text):
    return parse(list(lex(raw_text)))


def parse_lambda_raw(raw_text):
    raise Exception("Can't parse \\x.x lambda use blc.")


def pprint(parse_tree):
    print(parse_tree)
    return parse_tree


def shift(tree, amt=1, depth=0):
    # Only shift free variables
    # (those not bound within this particular expression, i.e. whose value is
    #  >= depth)
    if type(tree) == int:
        if tree >= depth:
            return tree + amt
        else:
            return tree
    elif type(tree) == tuple:
        return (LAMBDA, shift(tree[1], amt=amt, depth=depth+1))
    elif type(tree) == list:
        a, b = tree
        return [shift(a, amt=amt, depth=depth),
                shift(b, amt=amt, depth=depth)]
    else:
        raise TypeError(tree)


def substitute(lhs, rhs, depth=0):
    """
    1. Replace de bruijn index bound by the outside lambda in lhs with rhs.
    2. Increment any indexes in rhs that are bound to lambdas outside of lhs
       shift up according to how deep rhs is inserted into lhs.
    3. Drop the external lambda and decrement any free variables of lhs.
    """
    # Traverse the tree in pre-order (root, left, right)
    # Walk the entire tree to replace subterms.
    result = None
    if type(lhs) == int:
        if lhs + 1 == depth:  # The depth we're looking for:
            result = rhs
        else:
            result = lhs
    elif type(lhs) == tuple:
        # we shift up rhs's free variables every time we push it into an
        # abstraction.
        result = (LAMBDA,
                  substitute(lhs[1],
                             shift(rhs, amt=+1),
                             depth=depth+1))
    elif type(lhs) == list:
        a, b = lhs
        result = [substitute(a, rhs, depth=depth),
                  substitute(b, rhs, depth=depth)]
    else:
        raise TypeError("Unknown type for substitution {}."
                        .format(lhs))

    # Pop off the lambda for the top expression
    if depth == 0:
        result = shift(result[1], amt=-1)
    return result


def normal_order_reduction(tree, verbose=False):
    # choose the left-most redex first
    # substitute in the left hand side then reduce the rest
    if type(tree) == list:
        a, b = tree
        if type(a) == list:
            return [normal_order_reduction(a, verbose=verbose), b]
        elif type(a) == tuple:
            return substitute(a, b)
        else:  # int
            assert type(a) == int
            return [a, normal_order_reduction(b, verbose=verbose)]
    elif type(tree) == tuple:
        return (LAMBDA, normal_order_reduction(tree[1], verbose=verbose))
    elif type(tree) == int:
        return tree
    else:
        raise TypeError("Unknown type passed to reducer '{}': {}."
                        .format(type(tree), tree))


# Set reduction strategy here.
beta_reduce = normal_order_reduction


def is_head_normal_form(parse_tree):
    return type(parse_tree) != list


def is_normal_form(tree, verbose=True):
    if type(tree) == list:
        if type(tree[0]) == tuple:
            return False
        else:
            return is_normal_form(tree[0]) and is_normal_form(tree[1])
    elif type(tree) == tuple:
        return is_normal_form(tree[1])
    elif type(tree) == int:
        return True
    else:
        raise Exception("Unknown type.")


def evaluate(parse_tree, until=is_normal_form, verbose=False,
             stop_if_looping=False, language=None,
             max_evaluations=200):
    """
    Until is some state like `is_normal_form`

    stop_if_looping is just a silly way for us to kill the program if a cycle
    is detected. No this doesn't work for all infinite loops, but it
    certainly does for most of them.

    max_evaluations: maximum number of steps we will do before halting.
    """
    idx = 0
    if stop_if_looping:
        observed_states = {}
    while not until(parse_tree):
        redex = beta_reduce(parse_tree, verbose=verbose)
        if stop_if_looping and observed_states.get(str(redex)):
            raise CycleException("We're looping. Cycle length: {}"
                                 .format(len(observed_states)))
        if verbose:
            tree = tree_to_lang(parse_tree, language=LAM)
            print("eval{}   {}".format(idx, tree))
        parse_tree = redex
        idx += 1
        if stop_if_looping:
            observed_states[str(redex)] = True
        if idx == max_evaluations:
            if verbose:
                sys.stderr.write("Reached maximum number of cycles.\n")
            break
    if verbose:
        tree = tree_to_lang(parse_tree, language=LAM)
        print("eval{}   {}".format(idx, tree))
    return parse_tree


def evaluate_generator(parse_tree, until=is_normal_form,
                       stop_if_looping=False, language=None,
                       verbose=False,
                       max_evaluations=2000):
    """
    Until is some state like `is_normal_form`

    stop_if_looping is just a silly way for us to kill the program if a cycle
    is detected. No this doesn't work for all infinite loops, but it
    certainly does for many of them.

    evaluate generator is like evaluate but results in a generator of every
    step instead of a single tree

    max_evaluations: maximum number of steps we will do before halting.
    """
    idx = 0
    if stop_if_looping:
        observed_states = {}
    while not until(parse_tree):
        idx += 1
        if idx == max_evaluations:
            if verbose:
                sys.stderr.write("Reached maximum number of cycles.\n")
            break
        redex = beta_reduce(parse_tree)
        if stop_if_looping and observed_states.get(str(redex)):
            raise CycleException("We're looping. Cycle length: {}"
                                 .format(len(observed_states)))
        tree = tree_to_lang(parse_tree, language=language)
        yield tree
        parse_tree = redex
        if stop_if_looping:
            observed_states[str(redex)] = True
    tree = tree_to_lang(parse_tree, language=language)
    yield tree


def tree_to_lang(parse_tree, language=None):
    if language == BLC:
        return tree_to_blc(parse_tree)
    elif language == LAM:
        return tree_to_lam(parse_tree)
    else:
        raise Exception("Unknown language: {}".format(language))


def tree_to_blc(parse_tree):
    def walker(tree):
        if type(tree) == int:
            yield int_to_debruijn(tree)
        elif type(tree) == tuple:
            yield NT_LAMBDA + tree_to_blc(tree[1])
        elif type(tree) == list:
            a, b = tree
            yield NT_APPLY + tree_to_blc(a) + tree_to_blc(b)
        else:
            raise TypeError("Unknown type for parse_tree {}"
                            .format(parse_tree))
    # We perform a pre-order traversal node,left,right to go from parse tree to
    # blc
    return ''.join(walker(parse_tree))


def tree_to_lam(parse_tree, depth=0, lambda_sym=NT_LAMBDA_LAM, debruijn=False):
    # currently supports only 26 vars...
    alphabet = 'xyzabcdefghijklmnopqrstuvw'

    def walker(tree):
        if type(tree) == int:
            if debruijn:
                yield str(tree)
            else:
                var = alphabet[depth-tree-1]
                yield var
        elif type(tree) == tuple:
            body = tree_to_lam(tree[1], depth=depth+1, debruijn=debruijn)
            if debruijn:
                yield '{} {}'.format(lambda_sym, body)
            else:
                var = alphabet[depth]
                yield '{}{}.{}'.format(lambda_sym, var, body)
        elif type(tree) == list:
            a, b = tree
            yield ('({} {})'
                   .format(tree_to_lam(a, depth=depth),
                           tree_to_lam(b, depth=depth)))
        else:
            raise TypeError("Unknown type for parse_tree {}"
                            .format(parse_tree))
    # We perform a pre-order traversal node,left,right to go from parse tree to
    # lambda calculus with debruijn indices
    return ''.join(walker(parse_tree))


def shell(language):
    pass


def lang_from_filename(target_file=None, language=None):
    if language:
        language = BLC if BLC in language.lower() else LAM
    elif target_file:
        language = os.path.splitext(target_file)[1].lower()[1:]

    if not language:
        language = DEFAULT_LANG

    return language


def read_file(input_file, language=None):
    language = lang_from_filename(input_file, language)
    if language == BLC:
        return parse_blc_raw(slurp(input_file))
    elif language == LAM:
        return parse_lambda_raw(slurp(input_file))
    else:
        raise ValueError("Unknown language: {}".format(language))


def write_file(target_file, lambda_expression, language=None):
    language = lang_from_filename(target_file, language)
    body = tree_to_lang(lambda_expression, language=language)
    header = EVALUATION_HEADER
    output = '{}\n{}\n'.format(header, body)
    return spit(target_file, output, executable=True)


def run_shell(language=None, ps1_post=DEFAULT_PS1):
    def ps1(pre, post):
        pre = LAMBDA if pre == LAM else pre
        return '{}{}'.format(pre, post)
    language = lang_from_filename(None, language)
    QUIT_CMD = 'exit()'
    while True:
        cmd = input(ps1(pre=language, post=ps1_post))
        if cmd == QUIT_CMD:
            break
        pprint(eval_string(cmd, language=language))


def run_compile(input_file, output_file, source_language=None,
                target_language=None):
    # read and parse file_a
    # compile to file_b
    return write_file(output_file,
                      read_file(input_file, language=source_language),
                      language=target_language)


def run_file(input_file, language=None, verbose=False):
    language = lang_from_filename(input_file, language)
    contents = slurp(input_file)
    return pprint(eval_string(contents, language=language, verbose=verbose))


def eval_string(contents, language=None, verbose=False):
    language = lang_from_filename(None, language=language)
    if language == BLC:
        parse_tree = parse_blc_raw(contents)
    elif language == LAM:
        parse_tree = parse_lambda_raw(contents)
    else:
        raise ValueError("Unknown language.")
    eval_tree = evaluate(parse_tree, language=language, verbose=verbose)
    return tree_to_lang(eval_tree, language=language)


def generate_expression(length=10):
    """
    Only left-skewed trees are generated, i.e.
        ((((foo bar) baz) qux) ...)
    """
    return generate_SKI(length=length, s_p=0.1, k_p=0.45, i_p=0.45)


def generate_SKI(length=10, s_p=0.1, k_p=0.1, i_p=0.8):
    """
    Only left-skewed trees are generated, i.e.
        ((((foo bar) baz) qux) ...)
    """
    # define the combinators
    S = '00000001011110100111010'
    K = '0000110'
    I = '0010' # noqa
    blc_apply = NT_APPLY
    # generate random combinators
    t = s_p + k_p + i_p
    normp = [s_p/t, k_p/t, i_p/t]
    rcombs = np.random.choice([S, K, I], size=(length,),
                              p=normp)
    # create a 'string' of combinators. See McLennan 1997.
    result = rcombs[0]
    for i in range(length-1):
        result = blc_apply + result + rcombs[i]
    return result


def generate_SK_deterministic(s_c, k_c):
    total_count = s_c + k_c
    S = '00000001011110100111010'
    K = '0000110'
    I = '0010'  # noqa
    blc_apply = NT_APPLY
    return blc_apply * (total_count - 1) + S*s_c + K*k_c


def show_tree(tree):
    def walk_tree(tr, parent):
        if type(tr) == int:
            n = anytree.Node(str(tr), parent=parent)
            return n
        elif type(tr) == tuple:
            a, b = tr
            n = anytree.Node(str(a), parent=parent)
            walk_tree(b, parent=n)
            return n
        elif type(tr) == list:
            a, b = tr
            n = anytree.Node('[', parent=parent)
            walk_tree(a, parent=n)
            walk_tree(b, parent=n)
            return n
        else:
            raise Exception
    n = walk_tree(tree, parent=None)
    for pre, fill, node in anytree.RenderTree(n):
        print("%s%s" % (pre, node.name))


def run_generate_dataset(output_file, target_language=BLC, count=5000000,
                         exp_length=25, verbose=False):
    """
    Generates a dataset of random lambda calculus reductions.

    Format:
        01000110100010 0100100010 0010
        0100100010 0010
        <space> = next evaluation
        <newline> = next sequence
    """
    with open(output_file, 'w+') as f:
        for i in tqdm.tqdm(range(count)):
            exp = generate_expression(length=exp_length)
            tree = parse_blc_raw(exp)
            if verbose:
                show_tree(tree)
            st = ' '.join(evaluate_generator(tree,
                                             language=target_language))
            f.write(st)
            f.write('\n')


numpy.set_printoptions(threshold=sys.maxsize)
width = 5
expr_length = 9
height = width
data_back = multiprocessing.Array(ctypes.c_int, width*height*width)
data = np.ctypeslib.as_array(data_back.get_obj())
data = data.reshape(height, width, width)
lock = multiprocessing.Lock()


def num_iters(s_k_i):
    s_count, k_count, i_count = s_k_i
    i_factor = i_count / width
    total_combs = s_count + k_count
    if not total_combs:
        return 0
    prog = generate_SKI(length=expr_length,
                        s_p=(1-i_factor)*(s_count/total_combs),
                        k_p=(1-i_factor)*(k_count/total_combs),
                        i_p=i_factor)
    tree = parse_blc_raw(prog)
    num_iters = len(list(evaluate_generator(tree, language=BLC)))
    lock.acquire()
    data[i_count][k_count][s_count] = num_iters
    lock.release()


def run_plot(output_file):
    pool = multiprocessing.Pool()
    pairs = [(s_count, k_count, i_count)
             for s_count in reversed(range(width))
             for k_count in reversed(range(height))
             for i_count in reversed(range(width))]

    for _ in tqdm.tqdm(pool.imap_unordered(num_iters, pairs),
                       total=len(pairs)):
        pass
    pool.close()
    pool.join()
    for i in range(width):
        plt.imshow(data[i], interpolation='nearest',
                   cmap=plt.get_cmap('plasma'))
        plt.savefig('output/{}.png'.format(str(i).zfill(10)))


if __name__ == '__main__':
    epilog = """
examples:
    blc foo.{lam}
    blc bar.{blc}
    blc shell --language {blc}
    {blc}> 0100100010
    0010
    blc shell --language {lam}
    {LAMBDA}> \\x \\y y
    """.format(lam=LAM, blc=BLC, LAMBDA=LAMBDA)
    parser = (argparse.ArgumentParser(
              description='An interpreter for the untyped lambda calculus.',
              epilog=epilog,
              formatter_class=argparse.RawDescriptionHelpFormatter))

    parser.add_argument('input_file', metavar='input_file',
                        nargs='?', default=None,
                        help='input file')

    parser.add_argument('output_file', metavar='output_file',
                        nargs='?', default=None,
                        help='target file')

    parser.add_argument('--language', metavar='lam|blc',
                        help='Language for shell or command.')

    parser.add_argument('--gen_dataset',
                        help='Generate a dataset. blc --gen_dataset foo.txt',
                        action='store_true')

    parser.add_argument('--plot',
                        help='Generate a plot X axis is S Y is K.',
                        action='store_true')

    parser.add_argument('--target-language', metavar='lam|blc',
                        help='Target language for assembler.')

    parser.add_argument('-c', metavar='command',
                        help='Command to run.')

    parser.add_argument('--verbose', help='Print evaluation steps.',
                        action='store_true')

    args = parser.parse_args()
    if args.plot:
        run_plot(output_file=args.input_file)
    elif args.gen_dataset:
        output_file = args.input_file
        run_generate_dataset(output_file, target_language=BLC)
    elif args.input_file and args.output_file:
        # compilation time
        run_compile(args.input_file, args.output_file,
                    source_language=args.language,
                    target_language=args.target_language)
    elif args.input_file == 'shell':
        run_shell(language=args.language)
    elif args.input_file:
        # run file
        run_file(args.input_file, verbose=args.verbose)
    elif args.c:
        # command time
        pprint(eval_string(args.c, language=args.language))
