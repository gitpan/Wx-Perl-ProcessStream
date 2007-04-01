#############################################################################
## Name:        Wx/Perl/ProcessStream.pm
## Purpose:     capture async process STDOUT/STDERR
## Author:      Mark Dootson
## Modified by:
## Created:     25/03/2007
## Copyright:   (c) 2007 Mark Dootson
## Licence:     This program is free software; you can redistribute it and/or
##              modify it under the same terms as Perl itself
#############################################################################

package Wx::Perl::ProcessStream;
# for pod
our $VERSION = '0.09';

=head1 NAME

Wx::Perl::ProcessStream - access IO of external processes via events

=head1 VERSION

Version 0.09

=head1 SYNOPSYS

    use Wx::Perl::ProcessStream qw( :everything );
    
    EVT_WXP_PROCESS_STREAM_STDOUT( $self, \&evt_process_stdout);
    EVT_WXP_PROCESS_STREAM_STDERR( $self, \&evt_process_stderr);
    EVT_WXP_PROCESS_STREAM_EXIT( $self, \&evt_process_exit);
    
    my $proc1 = Wx::Perl::ProcessStream->OpenProcess('perl -e"print qq($_\n) for(@INC);"', 'MyName1', $self);
    my $command = 'executable.exe parm1 parm2 parm3'
    my $proc2 = Wx::Perl::ProcessStream->OpenProcess($command, 'MyName2', $self);
    my @args = qw( executable.exe parm1 parm2 parm3 );
    my $proc3 = Wx::Perl::ProcessStream->OpenProcess(\@args, 'MyName2', $self);
        
    sub evt_process_stdout {
        my ($self, $event) = @_;
        $event->Skip(1);
        my $process = $event->GetProcess;
        my $line = $event->GetLine;
        
        if($line eq 'something we are waiting for') {
            $process->WriteProcess('a message to stdin');
            
            $process->CloseInput() if($finishedwriting);
        }
        ............
    }
    
    sub evt_process_exit {
        my ($self, $event) = @_;
        $event->Skip(1);
        my $process = $event->GetProcess;
        my $line = $event->GetLine;
        my @buffers = @{ $process->GetStdOutBuffer };
        my @errors = @{ $process->GetStdOutBuffer };
        my $exitcode = $process->GetExitCode;
        ............
        $process->Destroy;
    }
    

=head1 DESCRIPTION

This module provides the STDOUT, STDERR and exit codes of asynchronously running processes via events.
It may be used for long running or blocking processes that provide periodic updates on state via STDOUT. Simple IPC is possible via STDIN.

Do not use this module simply to collect the output of another process. For that, it is much simpler to do:

    my ($status, $output) = Wx::ExecuteStdout( 'perl -e"print qq($_\n) for(@INC);"' );


=head2 Wx::Perl::ProcessStream


=head3 Methods


=over 12

=item OpenProcess

Run an external process.
If the process is launched successfully, returns a Wx::Perl::ProcessStream::Process object.
If the process could not be launched, returns undef;

    my $process = Wx::Perl::ProcessStream->OpenProcess($command, $name, $eventhandler);

    $command      = command text (and parameters) you wish to run. You may also pass a
                    reference to an array containing the command and parameters.
    $name         = an arbitray name for the process.
    $eventhandler = the Wx object that will handle events for this process.
    $process      = Wx::Perl::ProcessStream::Process object

If the process could not be started then zero is returned.
You should destroy each process after it has completed. You can do this after receiving the exit event.


=item GetDefaultAppCloseAction

Returns the default on application close action that will be given to new processes.
When your application exits, any remaining Wx::Perl::ProcessStream::Process objects will be signalled to close.
The default signal is wxpSIGTERM but you can change this to wxpSIGKILL if you are sure this is what you want.
Whenever a mew process is opened, it is given the application close action returned by GetDefaultAppCloseAction.
You can also set the application close action at an individual process level.

    my $def-action = Wx::Perl::ProcessStream->SetDefaultAppCloseAction();

    $def-action will be one of wxpSIGTERM or wxpSIGKILL; (default wxpSIGTERM)


=item SetDefaultAppCloseAction

Sets the default on application close action that will be given to new processes.
See GetDefaultAppCloseAction.

    Wx::Perl::ProcessStream->SetDefaultAppCloseAction( $newdefaction );

    $newdefaction = one of wxpSIGTERM or wxpSIGKILL

=item GetPollInterval

Get the current polling interval. See SetPollInterval.

    $milliseconds = Wx::Perl::ProcessStream->GetPollInterval();

=item SetPollInterval

When all buffers are empty but there are still running external process, the module will pause before polling the processes again for output.
By default, the module waits for 500 milliseconds. You can set the value of this polling intrval with this method.
Internally, a Wx::Timer object is used to handle polling and the value you set here is passed directly to that.
The precision of the intervals is OS dependent.

    Wx::Perl::ProcessStream->SetPollInterval( $milliseconds );

    $milliseconds = number of milliseconds to wait when no buffer activity

=back

=head2 Wx::Perl::ProcessStream::Process

Returned from a call to Wx::Perl::ProcessStream->OpenProcess.
It is also available in event handlers using $event->GetProcess.

=head3 Methods

=over 12

=item CloseInput

Close the STDIN stream of the external process. (Some processes may not close until STDIN is closed.)

    $process->CloseInput();

=item GetAppCloseAction

Returns the current process signal that will used on application exit. Either wxpSIGTERM or wxpSIGKILL.
See SetAppCloseAction.

    my $action = $process->GetAppCloseAction();

=item GetExitCode

Returns the process exit code. It is undefined until a wxpEVT_PROCESS_STREAM_EXIT event has been received.

    my $exitcode = $process->GetExitCode();

=item GetProcessName

Returns the process name as passed to the OpenProcess constructor.

    my $processname = $process->GetProcessName();

=item GetStdErrBuffer

This returns a reference to an array containing all the lines sent by the process to stderr.
Calling this clears the process object internal stderr buffer.
(This has no effect on the actual process I/O buffers.)

    my $arryref = $process->GetStdErrBuffer();

=item GetStdOutBuffer

This returns a reference to an array containing all the lines sent by the process to stdout.
Calling this clears the process object internal stdout buffer.
(This has no effect on the actual process I/O buffers.)

    my $arryref = $process->GetStdOutBuffer();

=item GetProcessId

Returns the process id assigned by the system.

    my $processid = $process->GetProcessId();

=item IsAlive

Check if the process still exists in the system.
Returns 1 if process exists, 0 if process does not exist, and undefined if there was some problem in signaling the process. 

    my $isalive = $process->IsAlive();

=item KillProcess

Send a SIGKILL signal to the external process.

    $process->KillProcess();

=item SetAppCloseAction

When your application exits, any remaining Wx::Perl::ProcessStream::Process objects will be signaled to close.
The default signal is wxpSIGTERM but you can change this to wxpSIGKILL if you are sure this is what you want.

    $process->SetAppCloseAction( $newaction );

    $newaction = one of wxpSIGTERM, wxpSIGKILL

=item TerminateProcess

Send a SIGTERM signal to the external process.

    $process->TerminateProcess();

=item WriteProcess

Write to the STDIN of process.

    $process->WriteProcess( $writedata . "\n" );

    $writedata = The data you wish to write. Remember to add any appropriate line endings your external process may expect.

=back

=head2 Wx::Perl::ProcessStream::ProcessEvent

A Wx::Perl::ProcessStream::ProcessEvent is sent whenever an external process started with OpenProcess writes to STDOUT, STDERR or when the process exits.


=head3 Event Connectors

=over 12

=item EVT_WXP_PROCESS_STREAM_STDOUT

Install an event handler for an event of type wxpEVT_PROCESS_STREAM_STDOUT exported on request by this module.
The event subroutine will receive a Wx::Perl::ProcessStream::ProcessEvent for every line written to STDOUT by the external process.

    EVT_WXP_PROCESS_STREAM_STDOUT( $eventhandler, $codref );

=item EVT_WXP_PROCESS_STREAM_STDERR

Install an event handler for an event of type wxpEVT_PROCESS_STREAM_STDERR exported on request by this module.
The event subroutine will receive a Wx::Perl::ProcessStream::ProcessEvent for every line written to STDERR by the external process.

    EVT_WXP_PROCESS_STREAM_STDERR( $eventhandler, $codref );

=item EVT_WXP_PROCESS_STREAM_EXIT

Install an event handler for an event of type wxpEVT_PROCESS_STREAM_EXIT exported on request by this module.
The event subroutine will receive a Wx::Perl::ProcessStream::ProcessEvent when the external process exits.

    EVT_WXP_PROCESS_STREAM_EXIT( $eventhandler, $codref );

=back

=head3 Methods

=over 12

=item GetLine

For events of type wxpEVT_PROCESS_STREAM_STDOUT and wxpEVT_PROCESS_STREAM_STDERR this will return the line written by the process.

=item GetProcess

This returns the process that raised the event. If this is a wxpEVT_PROCESS_STREAM_EXIT event you should destroy the process with $process->Destroy; 

=back

=head1 COPYRIGHT & LICENSE

Copyright (C) 2007 Mark Dootson, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 ACKNOWLEDGEMENTS

Thanks to Johan Vromans for testing and suggesting a better interface.

=head1 AUTHOR

Mark Dootson, C<< <mdootson at cpan.org> >>

=head1 SEE ALSO

The distribution includes examples in the 'example' folder.
From the source root, run

    perl -Ilib example/psexample.pl

You can enter commands, execute them and view results.

You may also wish to consult the wxWidgets manuals for:

Wx::Process

Wx::Execute

Wx::ExecuteArgs

Wx::ExecuteCommand

Wx::ExecuteStdout 

Wx::ExecuteStdoutStderr



=cut

#-----------------------------------------------------
# PACKAGE Wx::Perl::ProcessStream
#-----------------------------------------------------

package Wx::Perl::ProcessStream;
use strict;
use Wx 0.50 qw( wxEXEC_ASYNC wxSIGTERM wxSIGKILL);
require Exporter;
use base qw(Exporter);
use Wx::Perl::Carp;

#-----------------------------------------------------
# check wxWidgets version
#-----------------------------------------------------
if( Wx::wxVERSION() lt '2.006003') {
    croak qq(Wx $Wx::VERSION compiled with $Wx::wxVERSION_STRING.\n\nMinimum wxWidgets version 2.6.3 required for Wx::Perl::ProcessStream $VERSION);
}

#-----------------------------------------------------
# initialise
#-----------------------------------------------------

our ($ID_CMD_EXIT, $ID_CMD_STDOUT, $ID_CMD_STDERR, $WXP_DEFAULT_CLOSE_ACTION);

$ID_CMD_EXIT = Wx::NewId();
$ID_CMD_STDOUT = Wx::NewId();
$ID_CMD_STDERR = Wx::NewId();
$WXP_DEFAULT_CLOSE_ACTION = wxSIGTERM;

our @EXPORT_OK = qw( wxpEVT_PROCESS_STREAM_EXIT
                     wxpEVT_PROCESS_STREAM_STDERR
                     wxpEVT_PROCESS_STREAM_STDOUT
                     EVT_WXP_PROCESS_STREAM_STDOUT
                     EVT_WXP_PROCESS_STREAM_STDERR
                     EVT_WXP_PROCESS_STREAM_EXIT
                     wxpSIGTERM
                     wxpSIGKILL
                    );
our %EXPORT_TAGS = ();
$EXPORT_TAGS{'everything'} = \@EXPORT_OK;

our $ProcHandler = Wx::Perl::ProcessStream::ProcessHandler->new();

sub wxpEVT_PROCESS_STREAM_EXIT () { $ID_CMD_EXIT }
sub wxpEVT_PROCESS_STREAM_STDERR () { $ID_CMD_STDERR }
sub wxpEVT_PROCESS_STREAM_STDOUT () { $ID_CMD_STDOUT }
sub wxpSIGTERM () { wxSIGTERM }
sub wxpSIGKILL () { wxSIGKILL }

sub EVT_WXP_PROCESS_STREAM_STDOUT ($$) { $_[0]->Connect(-1,-1,&wxpEVT_PROCESS_STREAM_STDOUT, $_[1] ) };
sub EVT_WXP_PROCESS_STREAM_STDERR ($$) { $_[0]->Connect(-1,-1,&wxpEVT_PROCESS_STREAM_STDERR, $_[1] ) };
sub EVT_WXP_PROCESS_STREAM_EXIT ($$) { $_[0]->Connect(-1,-1,&wxpEVT_PROCESS_STREAM_EXIT, $_[1] ) };

sub OpenProcess {
    my $class = shift;
    my( $command, $procname, $handler ) = @_;
     
    $procname ||= 'any';
    
    my $process = Wx::Perl::ProcessStream::Process->new( $procname, $handler );
    $process->Redirect();
    $process->SetAppCloseAction($WXP_DEFAULT_CLOSE_ACTION);
    my $procid = (ref $command eq 'ARRAY') ? Wx::ExecuteArgs( $command, wxEXEC_ASYNC, $process )
                                           : Wx::ExecuteCommand( $command, wxEXEC_ASYNC, $process );
    if($procid) {
        $process->__set_process_id( $procid );
        $ProcHandler->AddProc( $process );
        return $process;
    } else {
        return undef;
    }
}

sub SetDefaultAppCloseAction {
    my $class = shift;
    my $newaction = shift;
    $WXP_DEFAULT_CLOSE_ACTION = ($newaction == wxSIGTERM||wxSIGKILL) ?  $newaction : $WXP_DEFAULT_CLOSE_ACTION;
}

sub GetDefaultAppCloseAction {
    $WXP_DEFAULT_CLOSE_ACTION;
}

sub GetPollInterval {
    $ProcHandler->GetInterval();
}

sub SetPollInterval {
    my ($class, $interval) = @_;
    $ProcHandler->_set_poll_interval($interval);
}
    

#-----------------------------------------------------
# PACKAGE Wx::Perl::ProcessStream::ProcessHandler;
#
# Inherits from timer and cycles througn running
# processes raising events for STDOUT/STDERR/EXIT
#-----------------------------------------------------

package Wx::Perl::ProcessStream::ProcessHandler;
use Wx qw( wxSIGTERM wxSIGKILL);
use base qw( Wx::Timer );

sub DESTROY {
    my $self = shift;
    
    ## clear any live procs
    for my $process (@{ $self->{_procs} }) {
        my $procid = $process->GetProcessId() if($process->IsAlive());
        $process->Detach;
        Wx::Process::Kill($procid, $process->GetAppCloseAction());
    }       
    $self->SUPER::DESTROY if $self->can("SUPER::DESTROY");
    
}

sub new {
    my $self = shift->SUPER::new(@_);
    $self->{_procs} = [];
    $self->{_pollinterval} = 500;
    return $self;
}

sub _set_poll_interval {
    my $self = shift;
    $self->{_pollinterval} = shift;
    if($self->IsRunning()) {
        $self->Stop();
        $self->Start( $self->{_pollinterval} );
    }
}

sub Notify {
    my ($self ) = @_;
    return 1 if($self->{_notify_in_progress}); # do not re-enter notify proc
    $self->{_notify_in_progress} = 1;
    
    my @checkprocs = @{ $self->{_procs} };     # get the current list of procs
    $self->{_procs} = [];                      # clear permanent list
    
    my $continue = 1;
    
    my $oldpollinterval = $self->{_pollinterval}; # handle poll interval changes
    
    while( $continue ) {
        my @procsleft = ();
        my $blockreadlines = 0;
        for my $process (@checkprocs) {
            
            # process inout actions
            while( my $action = shift( @{ $process->{_await_actions} }) ) {
               
                if( $action->{action} eq 'terminate' ) {
                    $process->CloseOutput() if( defined(my $handle = $process->GetOutputStream() ) );
                    Wx::Process::Kill($process->GetProcessId(), wxSIGTERM);
                }elsif( $action->{action} eq 'kill' ) {
                    $process->CloseOutput() if( defined(my $handle = $process->GetOutputStream() ) );
                    Wx::Process::Kill($process->GetProcessId(), wxSIGKILL);
                }elsif( $action->{action} eq 'closeinput') {
                    $process->CloseOutput() if( defined(my $handle = $process->GetOutputStream() ) );
                } elsif( $action->{action} eq 'write') {
                    if(defined( my $fh = $process->GetOutputStream() )) {
                        print $fh $action->{writedata};
                    }
                }    
            }
            
            my $procexitcode = $process->GetExitCode();
            my $linedataread = 0;
            
            # STDERR
            if( $process->IsErrorAvailable() && defined( my $linebuffer = readline( $process->GetErrorStream() ) ) ){
                $linedataread = 1;
                $blockreadlines++;
                $linebuffer =~ s/(\r\n|\n)$//;
                my $event = Wx::Perl::ProcessStream::ProcessEvent->new( &Wx::Perl::ProcessStream::wxpEVT_PROCESS_STREAM_STDERR, -1 );
                push(@{ $process->{_stderr_buffer} }, $linebuffer);
                $event->SetLine( $linebuffer );
                $event->SetProcess( $process );
                $process->__get_handler()->AddPendingEvent($event);
            }
            
            # STDOUT
            if( $process->IsInputAvailable() && defined( my $linebuffer = readline( $process->GetInputStream() ) ) ){
                $linedataread = 1;
                $blockreadlines++;
                $linebuffer =~ s/(\r\n|\n)$//;
                my $event = Wx::Perl::ProcessStream::ProcessEvent->new( &Wx::Perl::ProcessStream::wxpEVT_PROCESS_STREAM_STDOUT, -1 );
                push(@{ $process->{_stdout_buffer} }, $linebuffer);
                $event->SetLine( $linebuffer );
                $event->SetProcess( $process );
                $process->__get_handler()->AddPendingEvent($event);
            }
            
            if(defined($procexitcode) && !$linedataread) {
                my $event = Wx::Perl::ProcessStream::ProcessEvent->new( &Wx::Perl::ProcessStream::wxpEVT_PROCESS_STREAM_EXIT, -1);
                $event->SetLine( undef );
                $event->SetProcess( $process );
                $process->__get_handler()->AddPendingEvent($event);
                
            } else {
                push( @procsleft, $process );
            }
        
        } # for my $process (@checkprocs) {
        
        my $procsadded = 0;
        
        # add any newly created procs
        my @newprocs = @{ $self->{_procs} };
        if( @newprocs ) {
            push(@procsleft, @newprocs);
            $self->{_procs} = [];
            $procsadded = 1
        }
        @checkprocs = @procsleft;
        
        # loop if we have new procs or read data
        if( $procsadded || $blockreadlines ) {
            $continue = 1;
        } else {
            $continue = 0;
        }
        #--------------------------------------------------
        # yield whilst $self->{_procs} is populated
        #--------------------------------------------------
        $self->{_procs} = \@checkprocs;
        Wx::wxTheApp->Yield();
        @checkprocs = @{ $self->{_procs} };
        $self->{_procs} = [];
        
    } # while( $continue ) {
    
    $self->{_procs} = \@checkprocs;
    $self->{_notify_in_progress} = 0;
    
    $self->Stop() unless( @checkprocs );
}

sub Start {
    my $self = shift;
    my @args = @_;
    $self->SUPER::Start(@args);   
}

sub Stop {
    my $self = shift;
    
    $self->SUPER::Stop();   
}

sub AddProc {
    my $self = shift;
    my $newproc = shift;
    push(@{ $self->{_procs} }, $newproc );
    $self->Start($self->{_pollinterval}) if(!$self->IsRunning());
}

#-----------------------------------------------------
# PACKAGE Wx::Perl::ProcessStream::Process
#
# Adds some extra methods to Wx::Process
#-----------------------------------------------------

package Wx::Perl::ProcessStream::Process;
use Wx qw(wxSIGTERM
          wxSIGKILL
          wxSIGNONE
          wxKILL_OK
          wxKILL_BAD_SIGNAL
          wxKILL_ACCESS_DENIED
          wxKILL_NO_PROCESS
          wxKILL_ERROR);

use base qw( Wx::Process );

sub new {
    my $class = shift;
    my $procname = shift;
    my $handler = shift;
    my $self = $class->SUPER::new(@_);
    $self->__set_process_name($procname);
    $self->__set_handler($handler);
    $self->{_await_actions} = [];
    $self->{_stderr_buffer} = [];
    $self->{_stdout_buffer} = [];
    return $self;
}

sub OnTerminate {
    my($self, $pid, $status) = @_;
    $self->__set_exit_code($status);   
}

sub __get_handler {
    my $self = shift;
    return $self->{_handler};
}

sub __set_handler {
    my $self = shift;
    $self->{_handler} = shift;
}

sub GetAppCloseAction {
    my $self = shift;
    return $self->{_closeaction};
}

sub SetAppCloseAction {
    my $self = shift;
    my $newaction = shift;
    $self->{_closeaction} = ($newaction == wxSIGTERM||wxSIGKILL) ?  $newaction : $self->{_closeaction};
}

sub GetProcessName {
    my $self = shift;
    return $self->{_procname};
}

sub __set_process_name {
    my $self = shift;
    $self->{_procname} = shift;
}

sub GetExitCode {
    my $self = shift;
    return $self->{_exitcode};
}

sub __set_exit_code {
    my $self = shift;
    $self->{_exitcode} = shift;
}

sub GetStdOutBuffer {
    my $self = shift;
    my @buffers = @{ $self->{_stdout_buffer} };
    $self->{_stdout_buffer} = [];
    return \@buffers;
}

sub GetStdErrBuffer {
    my $self = shift;
    my @buffers = @{ $self->{_stderr_buffer} };
    $self->{_stderr_buffer} = [];
    return \@buffers;
}

sub GetProcessId {
    my $self = shift;
    return $self->{_processpid};
}

sub __set_process_id {
    my $self = shift;
    $self->{_processpid} = shift;
}

sub TerminateProcess {
    my $self = shift;
    push(@{ $self->{_await_actions} }, { action => 'terminate', } );
}

sub KillProcess {
    my $self = shift;
    push(@{ $self->{_await_actions} }, { action => 'kill', } );
}

sub WriteProcess {
    my ($self, $writedata) = @_;
    push(@{ $self->{_await_actions} }, { action => 'write', writedata => $writedata } );
}

sub CloseInput {
    my $self = shift;
    push(@{ $self->{_await_actions} }, { action => 'closeinput', } );
}

sub IsAlive {
    my $self = shift;
    my $alive = Wx::Process::Kill( $self->GetProcessId(), wxSIGNONE );
    if($alive == wxKILL_NO_PROCESS) {
        return 0;
    } elsif( $alive == wxKILL_OK ) {
        return 1;
    } else {
        return undef;
    }
}

#-----------------------------------------------------
# PACKAGE Wx::Perl::ProcessStream::ProcessEvent
#
# STDOUT, STDERR, EXIT events
#-----------------------------------------------------

package Wx::Perl::ProcessStream::ProcessEvent;
use Wx;
use base qw( Wx::PlCommandEvent );

sub new {
    my( $class, $type, $id ) = @_;
    my $self = $class->SUPER::new( $type, $id );
    return $self;
}

sub GetLine {
    my $self = shift;
    return $self->{_outputline};
}

sub SetLine {
    my $self = shift;
    $self->{_outputline} = shift;
}

sub GetProcess {
    my $self = shift;
    return $self->{_process};
}

sub SetProcess {
    my $self = shift;
    $self->{_process} = shift;
}

sub Clone {
    my $self = shift;
    my $class = ref $self;
    my $clone = $class->new( $self->GetEventType(), $self->GetId() );
    $clone->SetLine( $self->GetLine );
    $clone->SetProcess( $self->GetProcess() );
    return $clone;
}

1;
__END__


