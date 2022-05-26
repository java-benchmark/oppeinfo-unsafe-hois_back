package ee.hitsa.ois.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import ee.hitsa.ois.domain.subject.studyperiod.SubjectStudyPeriod;

public interface SubjectStudyPeriodRepository extends JpaRepository<SubjectStudyPeriod, Long>, JpaSpecificationExecutor<SubjectStudyPeriod> {
}
