use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Wx::Perl::ProcessStream',
    AUTHOR              => 'Mark Dootson <mdootson@cpan.org>',
    ABSTRACT_FROM       => 'lib/Wx/Perl/ProcessStream.pm',
    VERSION_FROM        => 'lib/Wx/Perl/ProcessStream.pm',
    PL_FILES            => {},
    PREREQ_PM           => { Wx => 0.50,},
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Wx-Perl-ProcessStream-*' },
    #EXE_FILES           => ['bin/wxpdk', 'bin/wxpar'],
);

