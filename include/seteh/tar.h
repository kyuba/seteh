/*
 * This file is part of the kyuba.org Seteh project.
 * See the appropriate repository at http://git.kyuba.org/ for exact file
 * modification records.
*/

/*
 * Copyright (c) 2009, 2010, Kyuba Project Members
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
*/

#ifndef LIBSETEH_TAR_H
#define LIBSETEH_TAR_H

#include <curie/io.h>
#include <curie/regex.h>

#ifdef __cplusplus
extern "C" {
#endif

void tar_x    (struct io *io,
               void (*on_new_file)(const char *, struct io *, void *),
               void (*on_end_of_archive)(void *),
               void *aux);

void tar_x_rx (struct io *io,
               struct graph *filter,
               void (*on_new_file)(const char *, struct io *, void *),
               void (*on_end_of_archive)(void *),
               void *aux);

#ifdef __cplusplus
}
#endif

#endif
