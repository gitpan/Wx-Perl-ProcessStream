#!perl

package main;

use strict;
use Test::More tests => 32;
use lib 't';
use Wx;
use WxTesting qw( app_from_wxtesting_frame );

my $app = app_from_wxtesting_frame( 'ProcessStreamTestingFrame' );
$app->MainLoop;

package ProcessStreamTestingFrame;
use base qw(WxTesting::Frame);
use Wx::Perl::ProcessStream qw( :everything );
use Test::More;


sub new {
	my $class = shift;
	my $self = $class->SUPER::new( undef, -1, 'Testing Wx::Perl::ProcessStream ');
	EVT_WXP_PROCESS_STREAM_STDOUT( $self, \&evt_process);
    EVT_WXP_PROCESS_STREAM_STDERR( $self, \&evt_process);
    EVT_WXP_PROCESS_STREAM_EXIT( $self, \&evt_process);
	$self->{_stdout} = [];
	$self->{_stderr} = [];
	$self->{_exitcode} = undef;
	return $self;
}

sub RunTests {
	my $self = shift;	
    my $perl = $^X;
    
    # speed up tests
    Wx::Perl::ProcessStream->SetPollInterval(100);
    
	# test group 1
	my $cmd;

	if($^O =~ /^MSWin/) {
        $cmd = [ $perl, '-e', q("print 'HELLO WORLD', qq(\n);") ];
	} else {
		$cmd = [ $perl, '-e', q(print 'HELLO WORLD', qq(\n);) ];
	}

	my $process = $self->start_process( $cmd );
    ok( $process->IsAlive() );
    $self->wait_for_test_complete();
    is( $process->IsAlive(), 0 );
	is( $self->{_stdout}->[0], 'HELLO WORLD' );
	my $errs = join('', @{ $self->{_stderr} });
	$errs ||= '';
	is( $errs, '' );
	is( $self->{_exitcode}, 0 );
    is( $process->GetExitCode() , 0 );
    $process->Destroy;
    $process = undef;
    
    # test group 1a - arrref
    
	if($^O =~ /^MSWin/) {
        $cmd = [ $perl, '-e', q("print 'HELLO WORLD', qq(\n);") ];
	} else {
		$cmd = [ $perl, '-e', q(print 'HELLO WORLD', qq(\n);) ];
	}

	$process = $self->start_process( $cmd );
    ok( $process->IsAlive() );
    $self->wait_for_test_complete();
    is( $process->IsAlive(), 0 );
	is( $self->{_stdout}->[0], 'HELLO WORLD' );
	$errs = join('', @{ $self->{_stderr} });
	$errs ||= '';
	is( $errs, '' );
	is( $self->{_exitcode}, 0 );
    is( $process->GetExitCode() , 0 );
    $process->Destroy;
    $process = undef;    
	
	# test group 2
	$cmd = $perl . ' notarealtestascript.pl';
	$process = $self->start_process( $cmd );
    ok( $process->IsAlive() );
    $self->wait_for_test_complete();
    is( $process->IsAlive(), 0 );
	my $out = join('', @{ $self->{_stdout} });
	$out ||= '';
	is( $out, '' );
	$errs = join('', @{ $self->{_stderr} });
	$errs ||= '';
	isnt( $errs, '' );
	isnt( $self->{_exitcode}, 0 );
    $process->Destroy;
    $process = undef;
	
	# test group 3
	if($^O =~ /^MSWin/) {
        $cmd = [ $perl, '-e', q("$|=1;print 'ONE', qq(\n);sleep 1;print 'TWO', qq(\n);sleep 1;print 'THREE',qq(\n);sleep 1;print STDERR 'FOUR', qq(\n);exit(5);") ];
	} else {
		$cmd = [ $perl, '-e', q($|=1;print 'ONE', qq(\n);sleep 1;print 'TWO', qq(\n);sleep 1;print 'THREE',qq(\n);sleep 1;print STDERR 'FOUR', qq(\n);exit(5);) ];
	}
	$process = $self->start_process( $cmd );
    ok( $process->IsAlive() );
    $self->wait_for_test_complete();
    is( $process->IsAlive(), 0 );
    my $bufferline = join('-', @{ $self->{_stdout } });
    $bufferline =~ s/^\-+//;
    $bufferline =~ s/\-+$//;
    is($bufferline, 'ONE-TWO-THREE' );
    $bufferline = join('-', @{ $self->{_stderr } });
    $bufferline =~ s/^\-+//;
    $bufferline =~ s/\-+$//;
    is($bufferline, 'FOUR' );
    is($self->{_exitcode}, 5 );
    $process->Destroy;
    $process = undef;
    
    # test group 4 - write STDIN
    $cmd = $perl . ' t/echo.pl';
	$process = $self->start_process( $cmd );
    ok( $process->IsAlive() );
    
    $process->WriteProcess( qq(TEST STDIN 1\n) );
    $process->WriteProcess( qq(TEST STDIN 2\n) );
    $process->CloseInput();
    $self->wait_for_test_complete();
    is( $process->IsAlive(), 0 );
    $bufferline = join('-', @{ $process->GetStdOutBuffer() });
    $bufferline =~ s/^\-+//;
    $bufferline =~ s/\-+$//;
    is($bufferline, 'ECHO:TEST STDIN 1-TEST STDIN 2' );
    $errs = join('', @{ $self->{_stderr} });
    $errs ||= '';
    is( $errs, '' );
    is( $process->GetExitCode(), 123 );
    $process->Destroy;
    $process = undef;
    
    # test group 5 - shell echo program 
    if($^O =~ /^MSWin/) {
        $cmd = 'cmd.exe /C ' . $perl . ' t/shelltest.pl';
    } else {
        $cmd = '/bin/sh t/shelltest.sh';
    }

    $process = $self->start_process( $cmd );
    ok( $process->IsAlive() );

    while(!defined($self->{_exitcode})) {
        if(join('-', @{ $process->GetStdOutBuffer() }) eq 'WXTEST INPUT') {
            $process->WriteProcess(qq(WX TEST DATA\n));
            $process->CloseInput();
        }
        Wx::wxTheApp->Yield();
        
        #sleep 1;
    }
    
    is( $process->IsAlive(), 0 );
    $bufferline = join('-', @{  $self->{_stdout} });
    $bufferline =~ s/^\-+//;
    $bufferline =~ s/\-+$//;
    is($bufferline, 'WXTEST INPUT-ECHO:WX TEST DATA' );
    $errs = join('', @{ $process->GetStdErrBuffer() });
    $errs ||= '';
    is( $errs, '' );
    is( $process->GetExitCode(), 0 );
    $process->Destroy;
    $process = undef;
    
	return 1;
}

sub start_process {
    my ($self, $cmd) = @_;
    $self->{_stdout} = [];
	$self->{_stderr} = [];
	$self->{_exitcode} = undef;
	my $process = Wx::Perl::ProcessStream->OpenProcess( $cmd, 'TestCmd', $self );
	die 'Failed to launch process' if(!$process);
    return $process;
}
    
sub wait_for_test_complete {
	my $self = shift;
	while(!defined($self->{_exitcode})) {
		Wx::wxTheApp->Yield();
		#sleep 1;
	}
}

sub evt_shell_stdout {
	my ($self, $event) = @_;
	$event->Skip(1);
    my $line = $event->GetLine();
    push(@{ $self->{_stdout} }, $line);
    my $process = $event->GetProcess();
    if($line eq 'WXTEST INPUT') {
        $process->WriteProcess( qq(WX TEST DATA\n));
        $process->CloseInput();
    }
}
	
sub evt_process {
	my ($self, $event) = @_;
	$event->Skip(1);
	
	my $evttype = $event->GetEventType();
	my $line = $event->GetLine();
    my $process = $event->GetProcess();
    # calling with perl one liners confuses line endings
	
	if($evttype == wxpEVT_PROCESS_STREAM_STDOUT) {
		push(@{ $self->{_stdout} }, $line);
	} elsif ( $evttype == wxpEVT_PROCESS_STREAM_STDERR) {
		push(@{ $self->{_stderr} }, $line);
	} elsif ( $evttype == wxpEVT_PROCESS_STREAM_EXIT) {
		$self->{_exitcode} = $process->GetExitCode();
	}
}

1;
