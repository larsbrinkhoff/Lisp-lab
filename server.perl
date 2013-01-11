#!/usr/bin/perl -w

use strict;
use Socket;
use Fcntl;
use MIME::Base64;

$/ = "\r\n";

sub sigpipe
{
    print "Sigpipe\n";
    print "Close client at port 22\n";
    close CLIENT;
}

$SIG{'PIPE'} = \&sigpipe;

my $port = 8080;
my $proto = getprotobyname('tcp');

# create a socket, make it reusable
socket(SERVER, PF_INET, SOCK_STREAM, $proto) or die "socket: $!";
setsockopt(SERVER, SOL_SOCKET, SO_REUSEADDR, 1) or die "setsock: $!";

# grab a port on this machine
my $paddr = sockaddr_in($port, INADDR_ANY);

# bind to a port, then listen
bind(SERVER, $paddr) or die "bind: $!";
listen(SERVER, SOMAXCONN) or die "listen: $!";
print "SERVER started on port $port\n";

$|++;

# accepting a connection
my $client_addr;
while ($client_addr = accept(HTTP, SERVER))
{
    if (not defined fileno(CLIENT))
    {
	print "Opening client at baker:22\n";
	my $proto = getprotobyname('tcp');
	my $iaddr = inet_aton("baker");
	my $paddr = sockaddr_in(22, $iaddr);
	socket(CLIENT, PF_INET, SOCK_STREAM, $proto) or die "socket: $!";
	connect(CLIENT, $paddr) or die "connect: $!";
	binmode(CLIENT, ":unix:raw");
	my $flags = fcntl(CLIENT, F_GETFL, 0);
	fcntl(CLIENT, F_SETFL, $flags | O_NONBLOCK);
	print "Done\n";
    }

    binmode(HTTP, ":unix:raw");
    $|++;

    # find out who connected
    my ($client_port, $client_ip) = sockaddr_in($client_addr);
    my $client_ipnum = inet_ntoa($client_ip);
    my $client_host = gethostbyaddr($client_ip, AF_INET);
    # print who has connected
    print "got a connection from: $client_host","[$client_ipnum]\n";

    while(1) {
	my $request = <HTTP>;
	last if not defined $request;

	print $request;
	my $line;
	my %header;
	while ($line = <HTTP>)
	{
	    chomp $line;
	    last if $line eq "";
	    my ($key, $val) = split(/: /, $line, 2);
	    $header{$key} = $val;
	    print " $key = $val\n";
	}

	if ($header{'X-My-Stuff'})
	{
	    my $n = 0;
	    foreach (split(/ /, $header{'X-My-Stuff'}))
	    {
		print CLIENT chr($_);
		$n++;
	    }
	}

	if ($header{'X-B64'})
	{
	    print CLIENT decode_base64($header{'X-B64'});
	}

	exit 0 if $header{'X-End'};

	if ($header{'X-Close'})
	{
	    print "Close client at port 22\n";
	    close CLIENT;
	    print HTTP "HTTP/1.0 200 OK\r\n";
	    print HTTP "Content-Length: 0\r\n";
	    print HTTP "Connection: close\r\n";
	    print HTTP "\r\n";
	    last;
	}

#	my $t = time();
#	my $input;
#	my $n = 0;
#	while ($n == 0 && (time() - $t) < 1)
# 	{
# 	    $n = read CLIENT, $input, 1024;
# 	    if (not defined $n)
# 	    {
# 		$n = 0;
# 		$input = "";
# 	    }
# 	}

	my $bits = '';
	vec($bits, fileno(CLIENT), 1) = 1;
#	vec($bits, fileno(HTTP), 1) = 1;
	select $bits, undef, undef, 0.5;
	my $input = "";
	my $n = 0;
	if (vec($bits, fileno(CLIENT), 1) == 1)
	{
	    print "Data from CLIENT\n";
	    $n = read CLIENT, $input, 1024;
	}
# 	if (vec($bits, fileno(HTTP), 1) == 1)
# 	{
# 	    print "Data from HTTP\n";
# 	}

	print HTTP "HTTP/1.0 200 OK\r\n";
	print HTTP "Content-Length: $n\r\n";
	print HTTP "Connection: keep-alive\r\n";
	print HTTP "Cache-Control: no-cache\r\n";
	print HTTP "\r\n";
	print HTTP "$input";
    }

    print "Closed HTTP connection\n";
    close HTTP;
}
