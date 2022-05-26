package ee.hitsa.ois.report;

import static ee.hitsa.ois.util.TranslateUtil.name;

import ee.hitsa.ois.domain.protocol.ProtocolStudent;
import ee.hitsa.ois.domain.student.StudentGroup;
import ee.hitsa.ois.enums.Language;
import ee.hitsa.ois.util.PersonUtil;

public class ProtocolStudentReport {
    private final String fullname;
    private final String grade;
    private final String gradeName;
    private final String studentgroup;

    ProtocolStudentReport(ProtocolStudent protocolStudent, Boolean letterGrades, Language lang) {
        fullname = PersonUtil.fullname(protocolStudent.getStudent().getPerson());
        StudentGroup sg = protocolStudent.getStudent().getStudentGroup();
        if (sg != null) {
            studentgroup = sg.getCode();
        } else {
            studentgroup = null;
        }
        if (protocolStudent.getGrade() != null) {
            grade = ReportUtil.gradeValue(protocolStudent.getGrade(), letterGrades, lang);
            gradeName = name(protocolStudent.getGrade(), lang);
        } else {
            grade = null;
            gradeName = null;
        }
    }

    public String getFullname() {
        return fullname;
    }

    public String getGrade() {
        return grade;
    }

    public String getGradeName() {
        return gradeName;
    }

    public String getStudentgroup() {
        return studentgroup;
    }
}
