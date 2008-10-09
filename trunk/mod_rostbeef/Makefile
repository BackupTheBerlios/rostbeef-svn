#Change /opt/lib/erlang/ in the two lines below to wherever your erlang installation is.
ERLANG_CFLAGS = -I/usr/lib/erlang/lib/erts-5.5.2/include -I/root/ejabberd-2.0.2/src
ERLANG_LIBS = -L/usr/lib/erlang/lib/erts-5.5.2/lib -lerl_interface -lei

EFLAGS = -I . -pz .. -pa . 

SOURCES = $(wildcard *.erl)
#Change that to match your ejabberd beam dir
OUTDIR = /usr/lib/ejabberd/ebin/


BEAMS = $(addprefix $(OUTDIR)/,$(SOURCES:.erl=.beam))

all: $(BEAMS)

$(OUTDIR)/%.beam:	%.erl
	erlc -W $(EFLAGS) -o $(OUTDIR) $<

clean:
	rm -f $(BEAMS)

