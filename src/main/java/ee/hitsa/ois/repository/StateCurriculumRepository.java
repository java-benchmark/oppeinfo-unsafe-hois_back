package ee.hitsa.ois.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import ee.hitsa.ois.domain.statecurriculum.StateCurriculum;

public interface StateCurriculumRepository extends JpaRepository<StateCurriculum, Long>, JpaSpecificationExecutor<StateCurriculum> {

    // XXX not used?
    @Query(value = "select * from (select sc.id, (select cc.connect_classifier_code from classifier_connect as cc where cc.main_classifier_code = 'EKR' and cc.classifier_code in (select sco.occupation_code from state_curriculum_occupation as sco where sc.id = sco.state_curriculum_id order by sco.id limit 1) ) as ekr_level from state_curriculum as sc) as a where ekr_level is not null", nativeQuery = true)
    List<Object[]> getEkrLEvels();
}
