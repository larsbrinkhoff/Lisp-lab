#!/usr/bin/perl -w

use strict;
use Socket;
use Fcntl;
use MIME::Base64;

# initialize host and port
my $host = shift || 'www.nocrew.org';
my $port = shift || 80;
$/ = "\r\n";
my $http_request = "GET / HTTP/1.0\r\n";

sub open_socket()
{
    my $proto = getprotobyname('tcp');
    socket(SERVER, PF_INET, SOCK_STREAM, $proto) or die "socket: $!";
    setsockopt(SERVER, SOL_SOCKET, SO_REUSEADDR, 1) or die "setsock: $!";
    my $paddr = sockaddr_in(2222, INADDR_ANY);
    bind(SERVER, $paddr) or die "bind: $!";
    listen(SERVER, 1) or die "listen: $!";
    $|++;
    accept(CLIENT, SERVER);
    binmode(CLIENT, ":unix:raw");
    my $flags = fcntl(CLIENT, F_GETFL, 0);
    fcntl(CLIENT, F_SETFL, $flags | O_NONBLOCK);
}

sub open_http()
{
    print " [#";
    my $proto = getprotobyname('tcp');
    my $iaddr = inet_aton($host);
    my $paddr = sockaddr_in($port, $iaddr);
    socket(HTTP, PF_INET, SOCK_STREAM, $proto) or die "socket: $!";
    connect(HTTP, $paddr) or die "connect: $!";
    binmode(HTTP, ":unix:raw");
    print "] ";
}

sub send_header
{
    my $input = shift;
    print HTTP "Host: $host\r\n";
    print HTTP "Connection: keep-alive\r\n";
    print HTTP "Cache-Control: no-cache\r\n";
    if (defined $input && length($input) > 0)
    {
	if (length($input) > 10)
	{
	    print HTTP "X-B64: ";
	    print HTTP encode_base64($input, '');
	}
	else
	{
	    print HTTP "X-My-Stuff:";
	    foreach (split(//, $input)) {
		my $x = ord($_);
		print HTTP " $x";
	    }
	}
	print HTTP "\r\n";
    }
    print HTTP "\r\n";
}

sub read_response
{
    my $status = <HTTP>;
    chomp $status;

    my %header;
    while (<HTTP>)
    {
	chomp;
	last if $_ eq "";
	my ($key, $val) = split(/: /, $_, 2);
	$header{$key} = $val;
    }

    return ($status, %header);
}

sub read_data {
    my %header = @_;
    my $data;
    if (exists $header{'Content-Length'})
    {
	my $length = $header{'Content-Length'};
	while ($length > 0)
	{
	    my $n = read HTTP, $data, $length;
	    last if $n == 0;
	    $length -= $n;
	    print CLIENT $data;
	}
    }
    elsif ($header{'Connection'} ne 'keep-alive')
    {
	while (<HTTP>)
	{
	    print CLIENT $_;
	}
    }
}

sub http_get {
    my $input = shift;

    open_http() if !defined(fileno HTTP);

    print HTTP $http_request;
    send_header($input);

    my $bits = '';
#    vec($bits, fileno(CLIENT), 1) = 1;
    vec($bits, fileno(HTTP), 1) = 1;
    select $bits, undef, undef, undef;
#     if (vec($bits, fileno(CLIENT), 1) == 1)
#     {
# 	print "Data from CLIENT\n";
# 	print HTTP "GET / HTTP/1.0\r\n";
# 	$http_request = "";
#     }
#    else
#     {
# 	$http_request = "GET / HTTP/1.0\r\n";
#     }

    my ($status, %header) = read_response();
    if ($status =~ /200/) {
	if ($header{'Content-Length'} eq '0' ||
	    not exists $header{'Content-Length'}) {
	    print '.';
	} else {
	    print "$header{'Content-Length'}/";
	}
    } else {
	print "$status\n";
	my $key;
	foreach $key (keys %header) {
	    print " $key = $header{$key}\n";
	}
    }

    read_data(%header);

    if ($header{'Connection'} eq 'close')
    {
	close HTTP or die "close: $!";
    }
}

sub sigpipe
{
    print "End!\n";

    open_http() if !defined(fileno HTTP);

    print HTTP "GET / HTTP/1.0\r\n";
    print HTTP "Host: $host\r\n";
    print HTTP "Connection: close\r\n";
    print HTTP "Cache-Control: no-cache\r\n";
    print HTTP "X-Close: yes\r\n";
    print HTTP "\r\n";

    my ($status, %header) = read_response();
    print "$status\n";
    my $key;
    foreach $key (keys %header) {
	print " $key = $header{$key}\n";
    }

#    while (<HTTP>)
#    {
#	chomp;
#	print " $_\n";
#    }
    close HTTP;
    close CLIENT;
    close SERVER;
    exit 0;
}

$SIG{'QUIT'} = \&sigpipe;
$SIG{'TERM'} = \&sigpipe;
$SIG{'PIPE'} = \&sigpipe;

open_socket();
while(1) {
    my $input;
    my $n = read CLIENT, $input, 256;
    if (defined $n)
    {
	sigpipe() if $n == 0;
    }
    http_get($input);
}
