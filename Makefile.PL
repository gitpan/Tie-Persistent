use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
	      'NAME'	=> 'Tie::Persistent',
	      'VERSION_FROM' => 'Persistent.pm', # finds $VERSION
	      'PREREQ_PM'    => { 'Storable' => '0.6',
                                  'Data::Dumper' => '2.09',
                                },
              'dist'         => { COMPRESS => 'gzip -9f', SUFFIX => 'gz' },
);
